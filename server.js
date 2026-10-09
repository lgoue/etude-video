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
alter table sessions add column if not exists rejected bool default false;`;

const DEF_INSTR = "You will watch several short videos of a conversational agent. Please watch each video in full with the sound on, then answer the questions that follow.\nThere are no right or wrong answers: we are interested in your personal impression.";
const DEF_COMP = [{ q: "What should you do after each video?", options: ["Answer the questions", "Skip straight to the next video", "Close the page"], correct: 0 }];

const DEF_CONSENT = "This study investigates how people perceive conversational agents. Your participation is voluntary and you may stop at any time by closing this page. Your responses are anonymous and will be used for research purposes only.";
const slugify = s => String(s || '').toLowerCase().trim().replace(/[^a-z0-9-]+/g, '-').replace(/^-+|-+$/g, '').slice(0, 40);
const shuffle = a => { for (let i = a.length - 1; i > 0; i--) { const j = Math.floor(Math.random() * (i + 1)); [a[i], a[j]] = [a[j], a[i]]; } return a; };
const FEATS = ['ability', 'gender', 'task', 'transcript'];

// Sélection équilibrée : priorité à l'équilibre intra-participant, puis aux vidéos les moins vues globalement
async function pick(e) {
  const vs = await q(`select v.*,(select count(*) from assignments a join sessions s on s.id=a.session_id where a.video_id=v.id and not s.rejected and (s.completed or s.created_at>now()-interval '2 hours'))::int n from videos v where experiment_id=$1`, [e.id]);
  const F = (e.balance || '').split(',').filter(Boolean);
  let pool_ = vs.filter(v => !v.attention); const att = shuffle(vs.filter(v => v.attention)).slice(0, e.n_attention), out = [], c = {};
  while (out.length < e.n_videos && pool_.length) {
    let best = null, bs = Infinity;
    for (const v of pool_) {
      const s = 1000 * F.reduce((t, f) => t + (c[f + ':' + v[f]] || 0), 0) + v.n + Math.random();
      if (s < bs) { bs = s; best = v; }
    }
    F.forEach(f => c[f + ':' + best[f]] = (c[f + ':' + best[f]] || 0) + 1);
    out.push(best); pool_ = pool_.filter(v => v !== best);
  }
  shuffle(out); att.forEach(a => out.splice(1 + Math.floor(Math.random() * out.length), 0, a));
  return out;
}

// ---- API participants
app.get('/api/public', async (_, res) => res.json(await q('select id,name,slug from experiments where active order by id')));
app.get('/api/public/:id', async (req, res) => {
  const [e] = await q(`select id,name,instructions,comprehension,consent,n_videos,n_attention,exists(select 1 from questions where experiment_id=experiments.id and kind like 'demo%') as has_demo,slug,(n_target>0 and (select count(*) from sessions s where s.experiment_id=experiments.id and not s.rejected and (s.completed or s.created_at>now()-interval '2 hours'))>=n_target) as full from experiments where (slug=$1 or id::text=$1) and active order by (slug=$1) desc limit 1`, [req.params.id]);
  if (!e) return res.status(404).json({});
  e.comprehension = e.comprehension.map(c => ({ q: c.q, options: c.options })); res.json(e);
});
app.post('/api/start', async (req, res) => {
  const { experiment_id, audio_ok, attempts, comp = [], consent, prolific: pl = {} } = req.body;
  const [e] = await q('select * from experiments where id=$1 and active', [experiment_id]); if (!e) return res.status(404).json({});
  if (e.n_target > 0 && (await q("select count(*)::int n from sessions s where s.experiment_id=$1 and not s.rejected and (s.completed or s.created_at>now()-interval '2 hours')", [e.id]))[0].n >= e.n_target) return res.json({ ok: false, full: true });
  if (e.consent && !consent) return res.status(400).json({});
  if (!e.comprehension.every((c, i) => comp[i] === c.correct)) return res.json({ ok: false });
  const vids = await pick(e), id = crypto.randomUUID();
  await q('insert into sessions(id,experiment_id,audio_ok,comp_attempts,consent,prolific_pid,prolific_study,prolific_session) values($1,$2,$3,$4,$5,$6,$7,$8)', [id, e.id, !!audio_ok, attempts || 1, !!consent, ...['pid', 'study', 'session'].map(k => String(pl[k] || '').slice(0, 100) || null)]);
  for (let i = 0; i < vids.length; i++) await q('insert into assignments values($1,$2,$3)', [id, vids[i].id, i]);
  const qs = await q('select id,kind,text,left_label,right_label,scale,options from questions where experiment_id=$1 order by pos', [e.id]);
  res.json({ ok: true, session: id, questions: qs.filter(x => !x.kind.startsWith('demo')), demo: qs.filter(x => x.kind.startsWith('demo')), videos: vids.map(v => ({ id: v.id, yt: v.yt, instruction: v.instruction })) });
});
app.post('/api/answer', async (req, res) => {
  const { session, video_id, answers, ended } = req.body;
  const [v] = await q('select v.* from videos v join assignments a on a.video_id=v.id where a.session_id=$1 and v.id=$2', [session, video_id]);
  if (!v) return res.status(400).json({});
  let ap = null;
  if (v.attention) { const [f] = await q("select id from questions where experiment_id=$1 and kind='likert' order by pos limit 1", [v.experiment_id]); ap = !!f && Number(answers[f.id]) === v.expected; }
  await q('insert into responses(session_id,video_id,answers,ended,attention_passed) values($1,$2,$3,$4,$5) on conflict(session_id,video_id) do update set answers=$3,ended=$4,attention_passed=$5', [session, video_id, answers, !!ended, ap]);
  res.json({ ok: true });
});
app.post('/api/finish', async (req, res) => {
  const [c] = await q('select (select count(*) from assignments where session_id=$1)::int a,(select count(*) from responses where session_id=$1)::int r,(select e.prolific_code from experiments e join sessions s on s.experiment_id=e.id where s.id=$1) code', [req.body.session]);
  if (!c || c.r < c.a) return res.json({ ok: false });
  await q('update sessions set completed=true where id=$1', [req.body.session]);
  res.json({ ok: true, code: c.code || '' });
});

app.post('/api/demo', async (req, res) => { await q('update sessions set demo=$1 where id=$2', [req.body.answers || {}, req.body.session]); res.json({ ok: true }); });

// Vérifie qu'une vidéo est accessible (YouTube via oEmbed, sinon requête HTTP partielle)
async function checkVideo(v) {
  const signal = AbortSignal.timeout(10000);
  try {
    if (/^[\w-]{11}$/.test(v.yt)) {
      const r = await fetch('https://www.youtube.com/oembed?format=json&url=' + encodeURIComponent('https://www.youtube.com/watch?v=' + v.yt), { signal });
      if (r.ok) return { ok: true, detail: 'YouTube : disponible et intégrable' };
      return { ok: false, detail: r.status === 401 ? 'intégration désactivée par le propriétaire' : r.status === 404 ? 'vidéo privée ou supprimée' : 'YouTube HTTP ' + r.status };
    }
    const r = await fetch(v.yt, { headers: { Range: 'bytes=0-1023' }, redirect: 'follow', signal });
    const ct = (r.headers.get('content-type') || '').toLowerCase();
    await r.body?.cancel();
    if (!r.ok) return { ok: false, detail: 'HTTP ' + r.status };
    if (ct.includes('text/html')) return { ok: false, detail: 'renvoie une page web (lien non public, protégé par mot de passe ou incorrect)' };
    if (!/^(video\/|application\/octet-stream)/.test(ct)) return { ok: false, detail: 'type de fichier inattendu : ' + (ct || 'inconnu') };
    return { ok: true, detail: ct + (r.status === 206 ? '' : ' (lecture partielle non supportée)') };
  } catch (e) { return { ok: false, detail: e.name === 'TimeoutError' ? 'délai dépassé' : 'inaccessible (' + (e.cause?.code || e.message) + ')' }; }
}

// ---- API admin
const A = '/api/admin';
app.get(A + '/experiments', async (_, res) => res.json(await q('select * from experiments order by id')));
app.post(A + '/experiments', async (req, res) => res.json((await q('insert into experiments(name,instructions,comprehension,consent) values($1,$2,$3,$4) returning *', [req.body.name || 'Nouvelle expérience', DEF_INSTR, JSON.stringify(DEF_COMP), DEF_CONSENT]))[0]));
app.put(A + '/experiments/:id', async (req, res) => {
  const b = req.body;
  const slug = slugify(b.slug);
  if (slug && (['admin', 'api'].includes(slug) || /^\d+$/.test(slug) || (await q('select 1 from experiments where slug=$1 and id<>$2', [slug, req.params.id])).length)) return res.json({ error: 'slug' });
  res.json((await q('update experiments set name=$1,active=$2,n_videos=$3,n_attention=$4,instructions=$5,comprehension=$6,consent=$7,balance=$8,prolific_code=$9,n_target=$10,slug=$11 where id=$12 returning *', [b.name, b.active, b.n_videos, b.n_attention, b.instructions, JSON.stringify(b.comprehension), b.consent || '', b.balance || '', (b.prolific_code || '').trim(), parseInt(b.n_target) || 0, slug || null, req.params.id]))[0]);
});
app.delete(A + '/experiments/:id', async (req, res) => { await q('delete from sessions where experiment_id=$1', [req.params.id]); await q('delete from experiments where id=$1', [req.params.id]); res.json({ ok: true }); });
async function runChecks(id) {
  const vs = await q('select id,yt from videos where experiment_id=$1 order by id', [id]), out = {};
  for (let i = 0; i < vs.length; i += 10) await Promise.all(vs.slice(i, i + 10).map(async v => {
    const r = out[v.id] = await checkVideo(v);
    await q('update videos set check_ok=$1,check_detail=$2,checked_at=now() where id=$3', [r.ok, r.detail, v.id]);
  }));
  return out;
}
app.post(A + '/experiments/:id/check', async (req, res) => res.json(await runChecks(req.params.id)));
// Vérification globale : vidéos accessibles + cohérence de la configuration
app.post(A + '/experiments/:id/verify', async (req, res) => {
  const id = req.params.id, it = [], add = (level, text) => it.push({ level, text });
  const [e] = await q('select * from experiments where id=$1', [id]); if (!e) return res.status(404).json({});
  const qs = await q('select kind,scale from questions where experiment_id=$1 order by pos', [id]);
  const chk = await runChecks(id), vs = await q('select * from videos where experiment_id=$1 order by id', [id]);
  const lab = v => /^[\w-]{11}$/.test(v.yt) ? v.yt : 'lien direct ' + v.yt.slice(0, 50);
  const bad = vs.filter(v => !chk[v.id].ok);
  bad.forEach(v => add('error', `Vidéo ${lab(v)} inaccessible : ${chk[v.id].detail}`));
  if (!vs.length) add('error', 'Aucune vidéo dans cette expérience');
  else if (!bad.length) add('ok', `${vs.length} vidéo(s), toutes accessibles`);
  const good = vs.filter(v => chk[v.id].ok), gn = good.filter(v => !v.attention).length, ga = good.filter(v => v.attention).length;
  add(gn >= e.n_videos ? 'ok' : 'error', `${gn} vidéo(s) accessible(s) hors pièges pour ${e.n_videos} par participant`);
  if (e.n_attention) add(ga >= e.n_attention ? 'ok' : 'error', `${ga} vidéo(s) piège accessible(s) pour ${e.n_attention} par participant`);
  for (const f of (e.balance || '').split(',').filter(Boolean)) { const n = vs.filter(v => !v.attention && !v[f]).length; if (n) add('warn', `${n} vidéo(s) sans valeur pour l'aspect équilibré « ${f} »`); }
  const first = qs.find(x => x.kind === 'likert');
  if (!qs.some(x => x.kind === 'likert' || x.kind === 'slider')) add('error', 'Aucune question Likert ou slider après les vidéos');
  else add('ok', `${qs.length} question(s) configurée(s)`);
  if (vs.some(v => v.attention)) {
    if (!first) add('error', 'Des vidéos pièges existent mais aucune question Likert ne permet de les valider');
    else vs.filter(v => v.attention && (v.expected < 1 || v.expected > first.scale)).forEach(v => add('error', `Piège ${lab(v)} : réponse attendue ${v.expected} hors de l'échelle 1–${first.scale}`));
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
app.get(A + '/experiments/:id/participants', async (req, res) => res.json(await q(`select s.id,s.rejected,s.completed,s.created_at,s.prolific_pid,s.audio_ok,s.comp_attempts,
  (select count(*) from sessions s2 where s2.experiment_id=s.experiment_id and s2.created_at<=s.created_at)::int num,
  (select count(*) from assignments a where a.session_id=s.id)::int n_assigned,
  (select count(*) from responses r where r.session_id=s.id)::int n_answered,
  (select count(*) from responses r where r.session_id=s.id and r.attention_passed is false)::int att_failed,
  (select count(*) from responses r where r.session_id=s.id and r.attention_passed is not null)::int att_total
  from sessions s where s.experiment_id=$1 order by s.created_at desc`, [req.params.id])));
app.put(A + '/sessions/:id', async (req, res) => { await q('update sessions set rejected=$1 where id=$2', [!!req.body.rejected, req.params.id]); res.json({ ok: true }); });
app.post(A + '/experiments/:id/reject-failed', async (req, res) => res.json({ n: (await q('update sessions set rejected=true where experiment_id=$1 and not rejected and exists(select 1 from responses r where r.session_id=sessions.id and r.attention_passed is false) returning id', [req.params.id])).length }));
app.get(A + '/summary', async (_, res) => res.json(await q(`select e.id,e.name,e.active,
  (select count(*) from sessions s where s.experiment_id=e.id)::int total,
  (select count(*) from sessions s where s.experiment_id=e.id and s.completed)::int done,
  (select count(*) from sessions s where s.experiment_id=e.id and s.rejected)::int rej,
  (select count(*) from responses r join videos v on v.id=r.video_id where v.experiment_id=e.id and r.session_id not in (select id from sessions where rejected))::int resp,
  (select count(*) from videos v where v.experiment_id=e.id and not v.attention)::int nv,
  (select count(*) from videos v where v.experiment_id=e.id and v.checked_at is not null and not v.check_ok)::int broken,
  (select min(c) from (select count(r.id) c from videos v left join responses r on r.video_id=v.id and r.session_id not in (select id from sessions where rejected) where v.experiment_id=e.id and not v.attention group by v.id) t)::int minr,
  (select max(c) from (select count(r.id) c from videos v left join responses r on r.video_id=v.id and r.session_id not in (select id from sessions where rejected) where v.experiment_id=e.id and not v.attention group by v.id) t)::int maxr
  from experiments e order by e.id`)));
app.get(A + '/experiments/:id', async (req, res) => {
  const id = req.params.id;
  res.json({
    questions: await q('select * from questions where experiment_id=$1 order by pos', [id]),
    videos: await q(`select v.*,(select count(*) from assignments a where a.video_id=v.id)::int assigned,(select count(*) from responses r where r.video_id=v.id and r.session_id not in (select id from sessions where rejected))::int answered,(select count(*) from responses r where r.video_id=v.id and r.attention_passed and r.session_id not in (select id from sessions where rejected))::int att_ok from videos v where experiment_id=$1 order by id`, [id]),
    sessions: (await q('select count(*)::int total,count(*) filter(where completed)::int done,count(*) filter(where exists(select 1 from responses r where r.session_id=sessions.id and r.attention_passed is false))::int attfail,count(*) filter(where rejected)::int rej,count(*) filter(where completed and not rejected)::int valid,count(*) filter(where not completed and not rejected and created_at>now()-make_interval(hours=>2))::int active from sessions where experiment_id=$1', [id]))[0],
    bal: await q('select f,v,sum(n)::int n from (' + ['ability', 'dimension', 'gender', 'task', 'transcript'].map(f => `select '${f}' f,coalesce(v.${f},'') v,count(r.id) n from videos v left join responses r on r.video_id=v.id and r.session_id not in (select id from sessions where rejected) where v.experiment_id=$1 and not v.attention group by v.${f}`).join(' union all ') + ') t group by f,v order by f,v', [id])
  });
});
app.post(A + '/experiments/:id/questions', async (req, res) => {
  const b = req.body; await q('insert into questions(experiment_id,kind,text,left_label,right_label,scale,options) values($1,$2,$3,$4,$5,$6,$7)', [req.params.id, b.kind, b.text, b.left_label || '', b.right_label || '', b.scale || 7, b.options || '']); res.json({ ok: true });
});
app.delete(A + '/questions/:id', async (req, res) => { await q('delete from questions where id=$1', [req.params.id]); res.json({ ok: true }); });
app.put(A + '/questions/:id', async (req, res) => {
  const b = req.body; await q('update questions set kind=$1,text=$2,left_label=$3,right_label=$4,scale=$5,options=$6 where id=$7', [b.kind, b.text, b.left_label || '', b.right_label || '', b.scale || 7, b.options || '', req.params.id]); res.json({ ok: true });
});
app.delete(A + '/videos/:id', async (req, res) => { await q('delete from videos where id=$1', [req.params.id]); res.json({ ok: true }); });
// Format : url;capacité;genre;tâche;transcript[;réponse attendue Likert (=piège);consigne]
function parseLine(l) {
  const p = l.split(';').map(x => x.trim());
  const m = p[0].match(/(?:v=|youtu\.be\/|embed\/|shorts\/)([\w-]{11})/) || p[0].match(/^([\w-]{11})$/);
  const yt = m ? m[1] : (/^https?:\/\//.test(p[0]) ? p[0].replace(/\/+$/, '').replace(/(\/s\/[\w-]+)$/, '$1/download') : null);
  if (!yt || p.length < 5) return null;
  const ex = parseInt(p[6]), a = !isNaN(ex);
  return [yt, p[1], p[2], p[3], p[4], p[5] || '', a, a ? ex : null, p.slice(7).join(';')];
}
app.post(A + '/experiments/:id/videos', async (req, res) => {
  let n = 0; const bad = [];
  for (const l of String(req.body.text).split('\n').map(x => x.trim()).filter(Boolean)) {
    const v = parseLine(l); if (!v) { bad.push(l); continue; }
    await q('insert into videos(experiment_id,yt,ability,gender,task,transcript,dimension,attention,expected,instruction) values($1,$2,$3,$4,$5,$6,$7,$8,$9,$10)', [req.params.id, ...v]); n++;
  }
  res.json({ added: n, bad });
});
app.put(A + '/videos/:id', async (req, res) => {
  const v = parseLine(String(req.body.text).trim()); if (!v) return res.json({ bad: true });
  await q('update videos set yt=$1,ability=$2,gender=$3,task=$4,transcript=$5,dimension=$6,attention=$7,expected=$8,instruction=$9,checked_at=null where id=$10', [...v, req.params.id]); res.json({ ok: true });
});
app.get(A + '/experiments/:id/export.csv', async (req, res) => {
  const id = req.params.id, qs = await q('select id,text from questions where experiment_id=$1 order by pos', [id]);
  const rows = await q('select r.*,(select count(*) from sessions s2 where s2.experiment_id=s.experiment_id and s2.created_at<=s.created_at)::int participant,v.yt,v.ability,v.gender,v.task,v.transcript,v.dimension,v.attention,s.audio_ok,s.comp_attempts,s.completed,s.rejected,s.consent,s.prolific_pid,s.prolific_study,s.prolific_session,s.demo,a.pos from responses r join videos v on v.id=r.video_id join sessions s on s.id=r.session_id join assignments a on a.session_id=r.session_id and a.video_id=r.video_id where v.experiment_id=$1 order by participant,a.pos', [id]);
  const cols = ['participant', 'prolific_pid', 'session_id', 'pos', 'yt', 'ability', 'gender', 'task', 'transcript', 'dimension', 'attention', 'attention_passed', 'ended', 'audio_ok', 'comp_attempts', 'completed', 'rejected', 'consent', 'prolific_study', 'prolific_session'];
  const esc = x => '"' + String(x ?? '').replace(/"/g, '""') + '"';
  res.type('text/csv').send('\ufeff' + [[...cols, ...qs.map(x => qs.filter(y => y.text === x.text).length > 1 ? x.text + ' (q' + x.id + ')' : x.text)].map(esc).join(','), ...rows.map(r => [...cols.map(c => r[c]), ...qs.map(x => r.answers[x.id] ?? (r.demo || {})[x.id])].map(esc).join(','))].join('\n'));
});

app.get('/admin', (_, res) => res.redirect('/admin.html'));
app.get('/:slug', (req, res) => req.params.slug.includes('.') ? res.sendStatus(404) : res.sendFile(path.join(__dirname, 'public', 'index.html')));

pool.query(SCHEMA).then(() => app.listen(process.env.PORT || 3000, () => console.log('OK'))).catch(e => { console.error(e); process.exit(1); });
