const express = require('express'), { Pool } = require('pg'), crypto = require('crypto'), path = require('path');
const url = process.env.DATABASE_URL;
const pool = new Pool({ connectionString: url, ssl: url && !/localhost|127\.0\.0\.1/.test(url) ? { rejectUnauthorized: false } : false });
const q = (s, p) => pool.query(s, p).then(r => r.rows);
const app = express(); app.use(express.json({ limit: '1mb' }));

const auth = (req, res, next) => {
  const pw = process.env.ADMIN_PASSWORD;
  if (!pw) return res.status(500).send('Variable ADMIN_PASSWORD manquante');
  const s = Buffer.from((req.headers.authorization || '').slice(6), 'base64').toString();
  if (s.slice(s.indexOf(':') + 1) === pw) return next();
  res.set('WWW-Authenticate', 'Basic realm="admin"').status(401).send('Authentification requise');
};
app.use(['/admin.html', '/api/admin'], auth);
app.use(express.static('public'));

const SCHEMA = `
create table if not exists experiments(id serial primary key,name text not null,active bool default true,n_videos int default 12,n_attention int default 2,instructions text default '',comprehension jsonb default '[]',created_at timestamptz default now());
create table if not exists questions(id serial primary key,experiment_id int references experiments(id) on delete cascade,kind text,text text,left_label text default '',right_label text default '',scale int default 7,pos serial);
create table if not exists videos(id serial primary key,experiment_id int references experiments(id) on delete cascade,yt text,ability text,gender text,task text,transcript text,attention bool default false,expected int,instruction text default '');
create table if not exists sessions(id text primary key,experiment_id int,audio_ok bool,comp_attempts int,completed bool default false,created_at timestamptz default now());
create table if not exists assignments(session_id text references sessions(id) on delete cascade,video_id int references videos(id) on delete cascade,pos int);
create table if not exists responses(id serial primary key,session_id text references sessions(id) on delete cascade,video_id int references videos(id) on delete cascade,answers jsonb,ended bool,attention_passed bool,created_at timestamptz default now(),unique(session_id,video_id));
alter table experiments add column if not exists consent text default '';
alter table questions add column if not exists options text default '';
alter table sessions add column if not exists consent bool;
alter table sessions add column if not exists demo jsonb default '{}';
alter table experiments add column if not exists balance text default 'ability,gender,task,transcript,dimension';
alter table videos add column if not exists dimension text default '';
alter table experiments add column if not exists prolific_code text default '';
alter table sessions add column if not exists prolific_pid text;
alter table sessions add column if not exists prolific_study text;
alter table sessions add column if not exists prolific_session text;
alter table videos add column if not exists check_ok bool;
alter table videos add column if not exists check_detail text;
alter table videos add column if not exists checked_at timestamptz;
alter table experiments add column if not exists n_target int default 0;
alter table experiments add column if not exists slug text;
create unique index if not exists experiments_slug_u on experiments(slug);
alter table sessions add column if not exists rejected bool default false;
alter table sessions add column if not exists abandoned bool default false;
alter table sessions add column if not exists last_seen timestamptz default now();
alter table videos add column if not exists name text default '';
alter table videos add column if not exists check_title text;
alter table videos add column if not exists part int default 1;
alter table questions add column if not exists part int default 1;
alter table experiments add column if not exists n_videos2 int default 0;
alter table experiments add column if not exists n_attention2 int default 0;
alter table experiments add column if not exists intro2 text default '';
alter table videos add column if not exists transcript_no text default '';
alter table videos add column if not exists model text default '';
alter table videos add column if not exists question_no text default '';
create table if not exists blocks(id serial primary key,experiment_id int references experiments(id) on delete cascade,title text default '',intro text default '',slot text default 'after',shuffle bool default false,pos serial);
alter table questions add column if not exists block_id int references blocks(id) on delete cascade;
alter table sessions add column if not exists blocks jsonb default '{}';
alter table questions add column if not exists section text default '';
alter table questions add column if not exists shuffle bool default false;`;

const DEF_INSTR = "You will watch several short videos of a conversational agent. Please watch each video in full with the sound on, then answer the questions that follow.\nThere are no right or wrong answers: we are interested in your personal impression.";
const DEF_COMP = [{ q: "What should you do after each video?", options: ["Answer the questions", "Skip straight to the next video", "Close the page"], correct: 0 }];

const DEF_CONSENT = "This study investigates how people perceive conversational agents. Your participation is voluntary and you may stop at any time by closing this page. Your responses are anonymous and will be used for research purposes only.";
const slugify = s => String(s || '').normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase().trim().replace(/[^a-z0-9-]+/g, '-').replace(/^-+|-+$/g, '').slice(0, 40);
// Adresse d'une étude : déduite du nom (minuscules, tirets), unique, jamais purement numérique
async function uniqueSlug(base, id) {
  let s = base || 'study';
  if (/^\d+$/.test(s) || ['admin', 'api'].includes(s)) s = 'study-' + s;
  let n = 1, c = s;
  while ((await q('select 1 from experiments where slug=$1 and id<>$2', [c, id])).length) c = s + '-' + (++n);
  return c;
}
async function backfillSlugs() {
  for (const e of await q(`select id,name from experiments where slug is null or slug = ''`)) await q('update experiments set slug=$1 where id=$2', [await uniqueSlug(slugify(e.name), e.id), e.id]);
}
const qScale = b => { const n = String(b.options || '').split(';').map(x => x.trim()).filter(Boolean).length; return b.kind === 'likert' && n >= 2 ? n : (b.scale || 7); };
// Questionnaires sans vidéo (échelles standardisées) : une page de questions à une position choisie
async function loadBlocks(eid) {
  const bs = await q('select id,title,intro,slot,shuffle from blocks where experiment_id=$1 order by pos', [eid]);
  const xs = await q('select id,kind,text,left_label,right_label,scale,options,section,shuffle,block_id from questions where experiment_id=$1 and block_id is not null order by pos', [eid]);
  return bs.map(b => ({ ...b, questions: xs.filter(x => x.block_id === b.id) })).filter(b => b.questions.length);
}
const shuffle = a => { for (let i = a.length - 1; i > 0; i--) { const j = Math.floor(Math.random() * (i + 1)); [a[i], a[j]] = [a[j], a[i]]; } return a; };
const FEATS = ['ability', 'gender', 'task', 'transcript'];
// Valeur d'une vidéo pour un aspect équilibré ; « model+ability » = combinaison croisée (chaque couple modèle × niveau)
const fval = (v, f) => f.split('+').map(k => v[k] || '').join(' × ');

// Sélection équilibrée : priorité à l'équilibre intra-participant, puis aux vidéos les moins vues globalement
async function pick(e, part = 1) {
  const NV = part === 2 ? e.n_videos2 : e.n_videos, NA = part === 2 ? e.n_attention2 : e.n_attention;
  const vs = await q(`select v.*,(select count(*) from assignments a join sessions s on s.id=a.session_id where a.video_id=v.id and not (s.rejected or s.abandoned) and (s.completed or s.last_seen>now()-interval '2 hours'))::int n from videos v where experiment_id=$1 and part=$2`, [e.id, part]);
  const F = (e.balance || '').split(',').filter(Boolean);
  let pool_ = vs.filter(v => !v.attention); const att = shuffle(vs.filter(v => v.attention)).slice(0, NA), out = [], c = {};
  while (out.length < NV && pool_.length) {
    let best = null, bs = Infinity;
    for (const v of pool_) {
      const s = 1000 * F.reduce((t, f) => t + (c[f + ':' + fval(v, f)] || 0), 0) + v.n + Math.random();
      if (s < bs) { bs = s; best = v; }
    }
    F.forEach(f => { const k = f + ':' + fval(best, f); c[k] = (c[k] || 0) + 1; });
    out.push(best); pool_ = pool_.filter(v => v !== best);
  }
  shuffle(out); att.forEach(a => out.splice(1 + Math.floor(Math.random() * out.length), 0, a));
  return out;
}

// ---- API participants
app.get('/api/public', async (_, res) => res.json(await q('select id,name,slug from experiments where active order by id')));
app.get('/api/public/:id', async (req, res) => {
  const [e] = await q(`select id,name,instructions,comprehension,consent,n_videos,n_attention,n_videos2,n_attention2,intro2,(select count(*) from blocks b where b.experiment_id=experiments.id and exists(select 1 from questions q where q.block_id=b.id))::int as n_blocks,exists(select 1 from questions where experiment_id=experiments.id and kind like 'demo%') as has_demo,slug,(n_target>0 and (select count(*) from sessions s where s.experiment_id=experiments.id and s.completed and not s.rejected and not s.abandoned)>=n_target) as full from experiments where (slug=lower($1) or id::text=$1) and active order by (slug=lower($1)) desc limit 1`, [req.params.id]);
  if (!e) return res.status(404).json({});
  e.comprehension = e.comprehension.map(c => ({ q: c.q, options: c.options })); res.json(e);
});
app.post('/api/start', async (req, res) => {
  const { experiment_id, audio_ok, attempts, comp = [], consent, prolific: pl = {} } = req.body;
  const [e] = await q('select * from experiments where id=$1 and active', [experiment_id]); if (!e) return res.status(404).json({});
  const done = e.n_target > 0 ? (await q('select count(*)::int n from sessions s where s.experiment_id=$1 and s.completed and not s.rejected and not s.abandoned', [e.id]))[0].n : 0;
  if (e.n_target > 0 && done >= e.n_target) return res.json({ ok: false, full: true });
  if (e.consent && !consent) return res.status(400).json({});
  if (!e.comprehension.every((c, i) => comp[i] === c.correct)) return res.json({ ok: false });
  const id = crypto.randomUUID(); let take = null;
  if (e.n_target > 0) {
    const [c] = await q('select count(*)::int n from sessions where experiment_id=$1 and not completed and not rejected and not abandoned and last_seen>now()-make_interval(hours=>2)', [e.id]);
    if (done + c.n >= e.n_target) take = (await q('update sessions set abandoned=true where id=(select id from sessions where experiment_id=$1 and not completed and not rejected and not abandoned and last_seen>now()-make_interval(hours=>2) order by last_seen limit 1 for update skip locked) returning id', [e.id]))[0]?.id;
  }
  const vids = take ? await q('select v.* from videos v join assignments a on a.video_id=v.id where a.session_id=$1 order by a.pos', [take]) : [...await pick(e, 1), ...await pick(e, 2)];
  await q('insert into sessions(id,experiment_id,audio_ok,comp_attempts,consent,prolific_pid,prolific_study,prolific_session) values($1,$2,$3,$4,$5,$6,$7,$8)', [id, e.id, !!audio_ok, attempts || 1, !!consent, ...['pid', 'study', 'session'].map(k => String(pl[k] || '').slice(0, 100) || null)]);
  for (let i = 0; i < vids.length; i++) await q('insert into assignments values($1,$2,$3)', [id, vids[i].id, i]);
  const qs = await q('select id,kind,text,left_label,right_label,scale,options,section,shuffle,part from questions where experiment_id=$1 and block_id is null order by pos', [e.id]);
  res.json({ ok: true, session: id, blocks: await loadBlocks(e.id), questions: qs.filter(x => !x.kind.startsWith('demo')), demo: qs.filter(x => x.kind.startsWith('demo')), videos: vids.map(v => ({ id: v.id, yt: v.yt, instruction: v.instruction, part: v.part })) });
});
app.post('/api/answer', async (req, res) => {
  const { session, video_id, answers, ended } = req.body;
  const [v] = await q('select v.* from videos v join assignments a on a.video_id=v.id where a.session_id=$1 and v.id=$2', [session, video_id]);
  if (!v) return res.status(400).json({});
  let ap = null;
  if (v.attention) { const fs = await q("select id from questions where experiment_id=$1 and kind='likert' and part=$2 and block_id is null", [v.experiment_id, v.part]); ap = fs.length > 0 && fs.every(f => Number(answers[f.id]) === v.expected); }
  await q('insert into responses(session_id,video_id,answers,ended,attention_passed) values($1,$2,$3,$4,$5) on conflict(session_id,video_id) do update set answers=$3,ended=$4,attention_passed=$5', [session, video_id, answers, !!ended, ap]);
  await q('update sessions set last_seen=now() where id=$1', [session]);
  res.json({ ok: true });
});
app.post('/api/block', async (req, res) => {
  const { session, block_id, answers, order } = req.body;
  const [b] = await q('select b.id from blocks b join sessions s on s.experiment_id=b.experiment_id where s.id=$1 and b.id=$2', [session, block_id]); if (!b) return res.status(400).json({});
  await q("update sessions set blocks=coalesce(blocks,'{}'::jsonb) || jsonb_build_object($2::text, $3::jsonb), last_seen=now() where id=$1", [session, String(block_id), JSON.stringify({ answers: answers || {}, order: order || [] })]);
  res.json({ ok: true });
});
// Reprise : un participant qui revient (même PROLIFIC_PID, ou même navigateur) retrouve sa session et ses réponses
app.post('/api/resume', async (req, res) => {
  const { experiment_id, pid, token } = req.body;
  const [e] = await q('select id,prolific_code from experiments where id=$1 and active', [experiment_id]); if (!e) return res.json({});
  const p = String(pid || '').slice(0, 100), t = String(token || '').slice(0, 64);
  if (p && (await q('select 1 from sessions where experiment_id=$1 and prolific_pid=$2 and completed and not rejected limit 1', [e.id, p])).length) return res.json({ finished: true, code: e.prolific_code || '' });
  const [s] = await q('select * from sessions where experiment_id=$1 and not completed and not rejected and ((prolific_pid is not null and prolific_pid=$2) or id=$3) order by created_at desc limit 1', [e.id, p, t]);
  if (!s) return res.json({});
  await q('update sessions set last_seen=now() where id=$1', [s.id]);
  const videos = await q('select v.id,v.yt,v.instruction,v.part from videos v join assignments a on a.video_id=v.id where a.session_id=$1 order by a.pos', [s.id]);
  const answered = (await q('select video_id from responses where session_id=$1', [s.id])).map(r => r.video_id);
  const qs = await q('select id,kind,text,left_label,right_label,scale,options,section,shuffle,part from questions where experiment_id=$1 and block_id is null order by pos', [e.id]);
  res.json({ resume: true, session: s.id, blocks: await loadBlocks(e.id), blocksDone: Object.keys(s.blocks || {}).map(Number), questions: qs.filter(x => !x.kind.startsWith('demo')), demo: qs.filter(x => x.kind.startsWith('demo')), demoDone: Object.keys(s.demo || {}).length > 0, videos, answered });
});
app.post('/api/finish', async (req, res) => {
  const [c] = await q('select (select count(*) from assignments where session_id=$1)::int a,(select count(*) from responses where session_id=$1)::int r,(select e.prolific_code from experiments e join sessions s on s.experiment_id=e.id where s.id=$1) code', [req.body.session]);
  if (!c || c.r < c.a) return res.json({ ok: false });
  await q('update sessions set completed=true where id=$1', [req.body.session]);
  res.json({ ok: true, code: c.code || '' });
});

app.post('/api/demo', async (req, res) => { await q('update sessions set demo=$1 where id=$2', [req.body.answers || {}, req.body.session]); res.json({ ok: true }); });

// Vérifie qu'une vidéo est accessible (YouTube via oEmbed, sinon requête HTTP partielle)
const normName = s => String(s || '').normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase().replace(/\.(mp4|m4v|mov|webm|mkv|avi)$/i, '').replace(/[^a-z0-9]+/g, ' ').trim();
async function checkVideo(v) {
  const signal = AbortSignal.timeout(10000), want = (v.name || '').trim();
  try {
    if (/^[\w-]{11}$/.test(v.yt)) {
      const r = await fetch('https://www.youtube.com/oembed?format=json&url=' + encodeURIComponent('https://www.youtube.com/watch?v=' + v.yt), { signal });
      if (!r.ok) return { ok: false, detail: r.status === 401 ? 'intégration désactivée par le propriétaire' : r.status === 404 ? 'vidéo privée ou supprimée' : 'YouTube HTTP ' + r.status };
      const title = String((await r.json()).title || '').trim();
      if (want && normName(want) !== normName(title)) return { ok: false, title, detail: `le titre YouTube « ${title} » ne correspond pas au nom saisi « ${want} »` };
      return { ok: true, title, detail: want ? 'YouTube : disponible, titre conforme au nom' : 'YouTube : disponible (aucun nom saisi, titre non vérifié)' };
    }
    const r = await fetch(v.yt, { headers: { Range: 'bytes=0-1023' }, redirect: 'follow', signal });
    const ct = (r.headers.get('content-type') || '').toLowerCase(), cd = r.headers.get('content-disposition') || '';
    await r.body?.cancel();
    if (!r.ok) return { ok: false, detail: 'HTTP ' + r.status };
    if (ct.includes('text/html')) return { ok: false, detail: 'renvoie une page web (lien non public, protégé par mot de passe ou incorrect)' };
    if (!/^(video\/|application\/octet-stream)/.test(ct)) return { ok: false, detail: 'type de fichier inattendu : ' + (ct || 'inconnu') };
    let file = (cd.match(/filename\*=(?:UTF-8'')?([^;]+)/i) || [])[1] || (cd.match(/filename="?([^";]+)"?/i) || [])[1] || '';
    try { file = decodeURIComponent(file.replace(/^"|"$/g, '')); } catch (e) { }
    if (!file) { try { const last = decodeURIComponent(new URL(r.url).pathname.split('/').pop() || ''); if (/\.[a-z0-9]{2,4}$/i.test(last)) file = last; } catch (e) { } }
    const base = ct + (r.status === 206 ? '' : ' (lecture partielle non supportée)');
    if (want && file && normName(want) !== normName(file)) return { ok: false, title: file, detail: `le fichier « ${file} » ne correspond pas au nom saisi « ${want} »` };
    if (want && !file) return { ok: true, detail: base + ' ; nom du fichier non déterminable, nom non vérifié' };
    return { ok: true, title: file, detail: base + (want ? ' ; nom conforme' : ' ; aucun nom saisi') };
  } catch (e) { return { ok: false, detail: e.name === 'TimeoutError' ? 'délai dépassé' : 'inaccessible (' + (e.cause?.code || e.message) + ')' }; }
}

// ---- API admin
const A = '/api/admin';
app.get(A + '/experiments', async (_, res) => res.json(await q('select * from experiments order by id')));
app.post(A + '/experiments', async (req, res) => {
  const [e] = await q('insert into experiments(name,instructions,comprehension,consent) values($1,$2,$3,$4) returning *', [req.body.name || 'Nouvelle étude', DEF_INSTR, JSON.stringify(DEF_COMP), DEF_CONSENT]);
  e.slug = await uniqueSlug(slugify(e.name), e.id);
  await q('update experiments set slug=$1 where id=$2', [e.slug, e.id]); res.json(e);
});
app.put(A + '/experiments/:id', async (req, res) => {
  const b = req.body;
  const slug = slugify(b.slug) || await uniqueSlug(slugify(b.name), req.params.id);
  if (slug && (['admin', 'api'].includes(slug) || /^\d+$/.test(slug) || (await q('select 1 from experiments where slug=$1 and id<>$2', [slug, req.params.id])).length)) return res.json({ error: 'slug' });
  res.json((await q('update experiments set name=$1,active=$2,n_videos=$3,n_attention=$4,instructions=$5,comprehension=$6,consent=$7,balance=$8,prolific_code=$9,n_target=$10,slug=$11,n_videos2=$12,n_attention2=$13,intro2=$14 where id=$15 returning *', [b.name, b.active, b.n_videos, b.n_attention, b.instructions, JSON.stringify(b.comprehension), b.consent || '', b.balance || '', (b.prolific_code || '').trim(), parseInt(b.n_target) || 0, slug || null, parseInt(b.n_videos2) || 0, parseInt(b.n_attention2) || 0, b.intro2 || '', req.params.id]))[0]);
});
app.delete(A + '/experiments/:id', async (req, res) => { await q('delete from sessions where experiment_id=$1', [req.params.id]); await q('delete from experiments where id=$1', [req.params.id]); res.json({ ok: true }); });
async function runChecks(id) {
  const vs = await q('select id,yt,name from videos where experiment_id=$1 order by id', [id]), out = {};
  for (let i = 0; i < vs.length; i += 10) await Promise.all(vs.slice(i, i + 10).map(async v => {
    const r = out[v.id] = await checkVideo(v);
    await q('update videos set check_ok=$1,check_detail=$2,check_title=$3,checked_at=now() where id=$4', [r.ok, r.detail, r.title || null, v.id]);
  }));
  return out;
}
app.post(A + '/experiments/:id/check', async (req, res) => res.json(await runChecks(req.params.id)));
// Vérification globale : vidéos accessibles + cohérence de la configuration
app.post(A + '/experiments/:id/verify', async (req, res) => {
  const id = req.params.id, it = [], add = (level, text) => it.push({ level, text });
  const [e] = await q('select * from experiments where id=$1', [id]); if (!e) return res.status(404).json({});
  const qs = await q('select kind,scale,part from questions where experiment_id=$1 and block_id is null order by pos', [id]);
  const chk = await runChecks(id), vs = await q('select * from videos where experiment_id=$1 order by id', [id]);
  const lab = v => (v.name ? v.name + ' (' : '(') + (/^[\w-]{11}$/.test(v.yt) ? v.yt : 'lien ' + v.yt.slice(0, 40)) + ')';
  const bad = vs.filter(v => !chk[v.id].ok);
  bad.forEach(v => add('error', `Vidéo ${lab(v)} : ${chk[v.id].detail}`));
  const noName = vs.filter(v => !v.name).length; if (noName) add('warn', `${noName} vidéo(s) sans nom : le titre n'est pas vérifié`);
  const dup = [...new Set(vs.map(v => normName(v.name)).filter((n, i, a) => n && a.indexOf(n) !== i))]; if (dup.length) add('warn', `Noms en double : ${dup.join(', ')}`);
  for (const [f, lbl] of [['transcript_no', 'numéro de transcript'], ['model', 'modèle'], ['question_no', 'numéro de question']]) { const n = vs.filter(v => !v[f]).length; if (n && n < vs.length) add('warn', `${n} vidéo(s) sans ${lbl} (renseigné pour les autres)`); }
  if (!vs.length) add('error', 'Aucune vidéo dans cette expérience');
  else if (!bad.length) add('ok', `${vs.length} vidéo(s), toutes accessibles et conformes`);
  for (const part of [1, 2]) {
    const has = vs.some(v => v.part === part), NV = part === 2 ? e.n_videos2 : e.n_videos, NA = part === 2 ? e.n_attention2 : e.n_attention;
    if (part === 2 && !NV) { if (has) add('warn', 'Partie 2 : des vidéos existent mais « vidéos par participant (partie 2) » vaut 0, donc la partie 2 est désactivée'); continue; }
    const tag = e.n_videos2 ? `Partie ${part} : ` : '', pv = vs.filter(v => v.part === part && chk[v.id].ok);
    const gn = pv.filter(v => !v.attention).length, ga = pv.filter(v => v.attention).length;
    add(gn >= NV ? 'ok' : 'error', `${tag}${gn} vidéo(s) accessible(s) hors pièges pour ${NV} par participant`);
    if (NA) add(ga >= NA ? 'ok' : 'error', `${tag}${ga} vidéo(s) piège accessible(s) pour ${NA} par participant`);
    if (e.n_videos2 && !qs.some(x => x.part === part && (x.kind === 'likert' || x.kind === 'slider'))) add('error', `${tag}aucune question Likert ou slider pour cette partie`);
    if (vs.some(v => v.part === part && v.attention) && !qs.some(x => x.part === part && x.kind === 'likert')) add('error', `${tag}des vidéos pièges existent mais aucune question Likert dans cette partie`);
  }
  for (const f of (e.balance || '').split(',').filter(Boolean)) { const n = vs.filter(v => !v.attention && f.split('+').some(k => !v[k])).length; if (n) add('warn', `${n} vidéo(s) sans valeur pour l'aspect équilibré « ${f} »`); }
  const lik = qs.filter(x => x.kind === 'likert'), first = lik.length ? { scale: Math.min(...lik.map(x => x.scale)) } : null;
  if (!qs.some(x => x.kind === 'likert' || x.kind === 'slider')) add('error', 'Aucune question Likert ou slider après les vidéos');
  else add('ok', `${qs.length} question(s) configurée(s)`);
  const bl = await q('select b.title,(select count(*) from questions where block_id=b.id)::int n from blocks b where b.experiment_id=$1 order by b.pos', [id]);
  bl.forEach(b => add(b.n ? 'ok' : 'warn', b.n ? `Questionnaire « ${b.title} » : ${b.n} item(s)` : `Questionnaire « ${b.title} » sans item : il ne sera pas affiché`));
  if (vs.some(v => v.attention)) {
    if (!first) add('error', 'Des vidéos pièges existent mais aucune question Likert ne permet de les valider');
    else vs.filter(v => v.attention && (v.expected < 1 || v.expected > Math.min(...lik.filter(x => x.part === v.part).map(x => x.scale), 99))).forEach(v => add('error', `Piège ${lab(v)} : réponse attendue ${v.expected} hors de l'échelle des questions Likert de sa partie`));
  }
  const comp = Array.isArray(e.comprehension) ? e.comprehension : [];
  if (!comp.length) add('warn', 'Aucune question de compréhension des consignes');
  else if (comp.some(c => !Array.isArray(c.options) || !Number.isInteger(c.correct) || c.correct < 0 || c.correct >= c.options.length)) add('error', 'Question de compréhension invalide : « correct » doit être l\'index d\'une option');
  else add('ok', `${comp.length} question(s) de compréhension valide(s)`);
  if (!e.consent) add('warn', 'Pas de texte de consentement');
  if (!e.prolific_code) add('warn', 'Pas de code de confirmation Prolific');
  if (!e.instructions) add('warn', 'Consignes vides');
  if (!e.active) add('warn', 'L\'expérience est inactive : les participants ne peuvent pas y accéder');
  res.json({ items: it });
});
app.get(A + '/experiments/:id/participants', async (req, res) => res.json(await q(`select s.id,s.rejected,s.abandoned,s.completed,s.created_at,s.last_seen,s.prolific_pid,s.audio_ok,s.comp_attempts,
  (select count(*) from sessions s2 where s2.experiment_id=s.experiment_id and s2.created_at<=s.created_at)::int num,
  (select count(*) from assignments a where a.session_id=s.id)::int n_assigned,
  (select count(*) from responses r where r.session_id=s.id)::int n_answered,
  (select count(*) from responses r where r.session_id=s.id and r.attention_passed is false)::int att_failed,
  (select count(*) from responses r where r.session_id=s.id and r.attention_passed is not null)::int att_total
  from sessions s where s.experiment_id=$1 order by s.created_at desc`, [req.params.id])));
app.put(A + '/sessions/:id', async (req, res) => { await q('update sessions set rejected=$1 where id=$2', [!!req.body.rejected, req.params.id]); res.json({ ok: true }); });
app.post(A + '/experiments/:id/reject-failed', async (req, res) => res.json({ n: (await q('update sessions set rejected=true where experiment_id=$1 and not rejected and exists(select 1 from responses r where r.session_id=sessions.id and r.attention_passed is false) returning id', [req.params.id])).length }));
app.post(A + '/experiments/:id/blocks', async (req, res) => res.json((await q('insert into blocks(experiment_id,title) values($1,$2) returning *', [req.params.id, String(req.body.title || '').trim() || 'Questionnaire']))[0]));
app.put(A + '/blocks/:id', async (req, res) => { const b = req.body; await q('update blocks set title=$1,intro=$2,slot=$3,shuffle=$4 where id=$5', [b.title || '', b.intro || '', ['before', 'between', 'after'].includes(b.slot) ? b.slot : 'after', !!b.shuffle, req.params.id]); res.json({ ok: true }); });
app.delete(A + '/blocks/:id', async (req, res) => { await q('delete from blocks where id=$1', [req.params.id]); res.json({ ok: true }); });
app.post(A + '/blocks/:id/move', async (req, res) => {
  const [m] = await q('select id,experiment_id,pos from blocks where id=$1', [req.params.id]); if (!m) return res.json({});
  const up = req.body.dir < 0;
  const [o] = await q(`select id,pos from blocks where experiment_id=$1 and pos ${up ? '<' : '>'} $2 order by pos ${up ? 'desc' : 'asc'} limit 1`, [m.experiment_id, m.pos]);
  if (o) { await q('update blocks set pos=$1 where id=$2', [o.pos, m.id]); await q('update blocks set pos=$1 where id=$2', [m.pos, o.id]); }
  res.json({ ok: true });
});
// Ajout en lot d'items : un par ligne, même type et mêmes libellés de boutons
app.post(A + '/blocks/:id/items', async (req, res) => {
  const [b] = await q('select id,experiment_id from blocks where id=$1', [req.params.id]); if (!b) return res.status(404).json({});
  const x = req.body, kind = x.kind === 'slider' ? 'slider' : 'likert', lines = String(x.text || '').split('\n').map(l => l.trim()).filter(Boolean);
  for (const t of lines) await q('insert into questions(experiment_id,kind,text,left_label,right_label,scale,options,section,shuffle,part,block_id) values($1,$2,$3,$4,$5,$6,$7,$8,false,1,$9)', [b.experiment_id, kind, t, x.left_label || '', x.right_label || '', qScale({ kind, options: x.options, scale: x.scale }), x.options || '', (x.section || '').trim(), b.id]);
  res.json({ added: lines.length });
});
// Copie d'une expérience : réglages, questions et vidéos (pas les participants ni les réponses). La copie est inactive et sans code Prolific.
app.post(A + '/experiments/:id/copy', async (req, res) => {
  const c = await pool.connect();
  try {
    const [src] = await q('select * from experiments where id=$1', [req.params.id]);
    if (!src) return res.status(404).json({});
    const name = String(req.body.name || '').trim() || src.name + ' (copie)', slug = await uniqueSlug(slugify(name), 0);
    await c.query('begin');
    const { rows: [e] } = await c.query(`insert into experiments(name,slug,active,n_videos,n_attention,instructions,comprehension,consent,balance,prolific_code,n_target,n_videos2,n_attention2,intro2)
      select $2::text,$3::text,false,n_videos,n_attention,instructions,comprehension,consent,balance,'',n_target,n_videos2,n_attention2,intro2 from experiments where id=$1 returning *`, [src.id, name, slug]);
    const bmap = {};
    for (const b of (await c.query('select * from blocks where experiment_id=$1 order by pos', [src.id])).rows) bmap[b.id] = (await c.query('insert into blocks(experiment_id,title,intro,slot,shuffle) values($1,$2,$3,$4,$5) returning id', [e.id, b.title, b.intro, b.slot, b.shuffle])).rows[0].id;
    for (const x of (await c.query('select * from questions where experiment_id=$1 order by pos', [src.id])).rows) await c.query('insert into questions(experiment_id,kind,text,left_label,right_label,scale,options,section,shuffle,part,block_id) values($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11)', [e.id, x.kind, x.text, x.left_label, x.right_label, x.scale, x.options, x.section, x.shuffle, x.part, x.block_id ? bmap[x.block_id] : null]);
    await c.query(`insert into videos(experiment_id,yt,ability,gender,task,transcript,dimension,attention,expected,instruction,name,part,transcript_no,model,question_no)
      select $2,yt,ability,gender,task,transcript,dimension,attention,expected,instruction,name,part,transcript_no,model,question_no from videos where experiment_id=$1 order by id`, [src.id, e.id]);
    await c.query('commit');
    res.json(e);
  } catch (err) { await c.query('rollback').catch(() => { }); console.error(err); res.status(500).json({ error: 'copy' }); }
  finally { c.release(); }
});
app.get(A + '/summary', async (_, res) => res.json(await q(`select e.id,e.name,e.active,
  (select count(*) from sessions s where s.experiment_id=e.id)::int total,
  (select count(*) from sessions s where s.experiment_id=e.id and s.completed and not s.abandoned)::int done,
  (select count(*) from sessions s where s.experiment_id=e.id and s.rejected)::int rej,
  (select count(*) from responses r join videos v on v.id=r.video_id where v.experiment_id=e.id and r.session_id not in (select id from sessions where rejected or abandoned))::int resp,
  (select count(*) from videos v where v.experiment_id=e.id and not v.attention)::int nv,
  (select count(*) from videos v where v.experiment_id=e.id and v.checked_at is not null and not v.check_ok)::int broken,
  (select min(c) from (select count(r.id) c from videos v left join responses r on r.video_id=v.id and r.session_id not in (select id from sessions where rejected or abandoned) where v.experiment_id=e.id and not v.attention group by v.id) t)::int minr,
  (select max(c) from (select count(r.id) c from videos v left join responses r on r.video_id=v.id and r.session_id not in (select id from sessions where rejected or abandoned) where v.experiment_id=e.id and not v.attention group by v.id) t)::int maxr
  from experiments e order by e.id`)));
app.get(A + '/experiments/:id', async (req, res) => {
  const id = req.params.id;
  res.json({
    questions: await q('select * from questions where experiment_id=$1 and block_id is null order by pos', [id]),
    blocks: await q('select * from blocks where experiment_id=$1 order by pos', [id]),
    items: await q('select * from questions where experiment_id=$1 and block_id is not null order by pos', [id]),
    videos: await q(`select v.*,(select count(*) from assignments a where a.video_id=v.id)::int assigned,(select count(*) from responses r where r.video_id=v.id and r.session_id not in (select id from sessions where rejected or abandoned))::int answered,(select count(*) from responses r where r.video_id=v.id and r.attention_passed and r.session_id not in (select id from sessions where rejected or abandoned))::int att_ok from videos v where experiment_id=$1 order by id`, [id]),
    sessions: (await q('select count(*)::int total,count(*) filter(where completed)::int done,count(*) filter(where exists(select 1 from responses r where r.session_id=sessions.id and r.attention_passed is false))::int attfail,count(*) filter(where rejected)::int rej,count(*) filter(where completed and not rejected and not abandoned)::int valid,count(*) filter(where not completed and not rejected and not abandoned and last_seen>now()-make_interval(hours=>2))::int active from sessions where experiment_id=$1', [id]))[0],
    bal: await q('select f,v,sum(n)::int n from (' + [['ability', 'v.ability'], ['dimension', 'v.dimension'], ['gender', 'v.gender'], ['task', 'v.task'], ['transcript', 'v.transcript'], ['model', 'v.model'], ['model × niveau', "coalesce(v.model,'') || ' × ' || coalesce(v.ability,'')"]].map(([f, x]) => `select '${f}' f,coalesce(${x},'') v,count(r.id) n from videos v left join responses r on r.video_id=v.id and r.session_id not in (select id from sessions where rejected or abandoned) where v.experiment_id=$1 and not v.attention group by ${x}`).join(' union all ') + ') t group by f,v order by f,v', [id])
  });
});
app.post(A + '/experiments/:id/questions', async (req, res) => {
  const b = req.body; await q('insert into questions(experiment_id,kind,text,left_label,right_label,scale,options,section,shuffle,part) values($1,$2,$3,$4,$5,$6,$7,$8,$9,$10)', [req.params.id, b.kind, b.text, b.left_label || '', b.right_label || '', qScale(b), b.options || '', (b.section || '').trim(), !!b.shuffle, b.part === 2 ? 2 : 1]); res.json({ ok: true });
});
app.delete(A + '/questions/:id', async (req, res) => { await q('delete from questions where id=$1', [req.params.id]); res.json({ ok: true }); });
app.put(A + '/questions/:id', async (req, res) => {
  const b = req.body; await q('update questions set kind=$1,text=$2,left_label=$3,right_label=$4,scale=$5,options=$6,section=$7,shuffle=$8,part=$9 where id=$10', [b.kind, b.text, b.left_label || '', b.right_label || '', qScale(b), b.options || '', (b.section || '').trim(), !!b.shuffle, b.part === 2 ? 2 : 1, req.params.id]); res.json({ ok: true });
});
app.post(A + '/questions/:id/move', async (req, res) => {
  const [m] = await q('select id,experiment_id,pos,kind,part,block_id from questions where id=$1', [req.params.id]); if (!m) return res.json({});
  const up = req.body.dir < 0, demo = m.kind.startsWith('demo');
  const [o] = await q(`select id,pos from questions where experiment_id=$1 and pos ${up ? '<' : '>'} $2 and (kind like 'demo%')=$3 and part=$4 and block_id is not distinct from $5 order by pos ${up ? 'desc' : 'asc'} limit 1`, [m.experiment_id, m.pos, demo, m.part, m.block_id]);
  if (o) { await q('update questions set pos=$1 where id=$2', [o.pos, m.id]); await q('update questions set pos=$1 where id=$2', [m.pos, o.id]); }
  res.json({ ok: true });
});
app.post(A + '/experiments/:id/section-shuffle', async (req, res) => { await q("update questions set shuffle=$1 where experiment_id=$2 and coalesce(section,'')=$3 and part=$4 and block_id is null and kind not like 'demo%'", [!!req.body.shuffle, req.params.id, req.body.section || '', req.body.part === 2 ? 2 : 1]); res.json({ ok: true }); });
app.delete(A + '/videos/:id', async (req, res) => { await q('delete from videos where id=$1', [req.params.id]); res.json({ ok: true }); });
// Import de vidéos : avec ligne d'en-tête (colonnes nommées, dans n'importe quel ordre) ou sans (ordre fixe).
// Séparateur : « ; » ou tabulation (copier-coller depuis un tableur).
const VCOLS = ['name', 'url', 'level', 'gender', 'task', 'transcript', 'dimension', 'transcript_no', 'model', 'question_no', 'expected', 'instruction'];
const VALIAS = { name: 'name nom', url: 'url lien link video', level: 'level niveau ability', gender: 'gender genre agent', task: 'task tache', transcript: 'transcript transcript_gender genre_transcript', dimension: 'dimension', transcript_no: 'transcript_no n_transcript numero_transcript transcript_number', model: 'model modele', question_no: 'question_no n_question numero_question question_number question', expected: 'expected attendu reponse_attendue', instruction: 'instruction consigne', part: 'part partie' };
const normCol = s => String(s || '').normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase().replace(/[^a-z0-9]+/g, '_').replace(/^_+|_+$/g, '');
const colOf = h => Object.keys(VALIAS).find(k => VALIAS[k].split(' ').includes(normCol(h)));
const splitLine = l => (l.includes('\t') ? l.split('\t') : l.split(';')).map(x => x.trim());
const isRef = s => /^https?:\/\//.test(s || '') || /^[\w-]{11}$/.test(s || '');
function toVideo(o) {
  const u = o.url || '', m = u.match(/(?:v=|youtu\.be\/|embed\/|shorts\/)([\w-]{11})/) || u.match(/^([\w-]{11})$/);
  const yt = m ? m[1] : (/^https?:\/\//.test(u) ? u.replace(/\/+$/, '').replace(/(\/s\/[\w-]+)$/, '$1/download') : null);
  if (!yt || !o.level || !o.gender || !o.task || !o.transcript) return null;
  const ex = parseInt(o.expected), a = !isNaN(ex), pt = parseInt(o.part);
  return { yt, ability: o.level, gender: o.gender, task: o.task, transcript: o.transcript, dimension: o.dimension || '', attention: a, expected: a ? ex : null, instruction: o.instruction || '', name: o.name || '', transcript_no: o.transcript_no || '', model: o.model || '', question_no: o.question_no || '', part: pt === 1 || pt === 2 ? pt : null };
}
function parseVideos(text) {
  const lines = String(text || '').split('\n').map(x => x.trim()).filter(Boolean), out = [], bad = [];
  const first = lines.length ? splitLine(lines[0]).map(colOf) : [];
  let cols = null;
  if (first.includes('url') && first.filter(Boolean).length >= 2) { cols = first; lines.shift(); }
  for (const l of lines) {
    let p = splitLine(l);
    if (!cols && !isRef(p[1])) p = ['', ...p];
    const o = {};
    (cols || VCOLS).forEach((c, i) => { if (c && p[i] !== undefined) o[c] = (c === 'instruction' && !cols) ? p.slice(i).join(';') : p[i]; });
    const v = toVideo(o); v ? out.push(v) : bad.push(l);
  }
  return { out, bad };
}
const VFIELDS = ['yt', 'ability', 'gender', 'task', 'transcript', 'dimension', 'attention', 'expected', 'instruction', 'name', 'transcript_no', 'model', 'question_no'];
app.post(A + '/experiments/:id/videos', async (req, res) => {
  const { out, bad } = parseVideos(req.body.text), dflt = req.body.part === 2 ? 2 : 1;
  for (const v of out) await q(`insert into videos(experiment_id,${VFIELDS.join(',')},part) values($1,${VFIELDS.map((_, i) => '$' + (i + 2)).join(',')},$${VFIELDS.length + 2})`, [req.params.id, ...VFIELDS.map(f => v[f]), v.part || dflt]);
  res.json({ added: out.length, bad });
});
app.put(A + '/videos/:id', async (req, res) => {
  const { out } = parseVideos(req.body.text); if (out.length !== 1) return res.json({ bad: true });
  const v = out[0];
  await q(`update videos set ${VFIELDS.map((f, i) => f + '=$' + (i + 1)).join(',')},part=$${VFIELDS.length + 1},checked_at=null where id=$${VFIELDS.length + 2}`, [...VFIELDS.map(f => v[f]), v.part || (req.body.part === 2 ? 2 : 1), req.params.id]);
  res.json({ ok: true });
});
app.get(A + '/experiments/:id/export.csv', async (req, res) => {
  const id = req.params.id, qs = await q(`select q.id,(case when b.title is null then q.text else b.title || ' | ' || q.text end) as text from questions q left join blocks b on b.id=q.block_id where q.experiment_id=$1 order by q.pos`, [id]);
  const rows = await q('select r.*,(select count(*) from sessions s2 where s2.experiment_id=s.experiment_id and s2.created_at<=s.created_at)::int participant,v.name as video_name,v.yt,v.part,v.ability,v.gender,v.task,v.transcript,v.transcript_no,v.model,v.question_no,v.dimension,v.attention,s.audio_ok,s.comp_attempts,s.completed,s.rejected,s.abandoned,s.consent,s.prolific_pid,s.prolific_study,s.prolific_session,s.demo,s.blocks,a.pos from responses r join videos v on v.id=r.video_id join sessions s on s.id=r.session_id join assignments a on a.session_id=r.session_id and a.video_id=r.video_id where v.experiment_id=$1 order by participant,a.pos', [id]);
  const cols = ['participant', 'prolific_pid', 'session_id', 'pos', 'part', 'video_name', 'yt', 'ability', 'gender', 'task', 'transcript', 'transcript_no', 'model', 'question_no', 'dimension', 'attention', 'attention_passed', 'ended', 'question_order', 'audio_ok', 'comp_attempts', 'completed', 'rejected', 'abandoned', 'consent', 'prolific_study', 'prolific_session'];
  rows.forEach(r => { r.question_order = (r.answers._order || []).join('|'); r.ba = Object.assign({}, ...Object.values(r.blocks || {}).map(b => b.answers || {})); });
  const esc = x => '"' + String(x ?? '').replace(/"/g, '""') + '"';
  res.type('text/csv').send('\ufeff' + [[...cols, ...qs.map(x => qs.filter(y => y.text === x.text).length > 1 ? x.text + ' (q' + x.id + ')' : x.text)].map(esc).join(','), ...rows.map(r => [...cols.map(c => r[c]), ...qs.map(x => r.answers[x.id] ?? (r.demo || {})[x.id] ?? r.ba[x.id])].map(esc).join(','))].join('\n'));
});

app.get('/admin', (_, res) => res.redirect('/admin.html'));
app.get('/:slug', (req, res) => req.params.slug.includes('.') ? res.sendStatus(404) : res.sendFile(path.join(__dirname, 'public', 'index.html')));

pool.query(SCHEMA).then(backfillSlugs).then(() => app.listen(process.env.PORT || 3000, () => console.log('OK'))).catch(e => { console.error(e); process.exit(1); });
