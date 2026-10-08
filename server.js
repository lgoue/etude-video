const express = require('express'), { Pool } = require('pg'), crypto = require('crypto');
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
create table if not exists responses(id serial primary key,session_id text references sessions(id) on delete cascade,video_id int references videos(id) on delete cascade,answers jsonb,ended bool,attention_passed bool,created_at timestamptz default now(),unique(session_id,video_id));`;

const DEF_INSTR = "Vous allez regarder plusieurs courtes vidéos d'un agent conversationnel. Regardez chaque vidéo en entier, avec le son activé, puis répondez aux questions qui suivent.\nIl n'y a pas de bonne ou de mauvaise réponse : nous nous intéressons à votre impression personnelle.";
const DEF_COMP = [{ q: "Que devez-vous faire après chaque vidéo ?", options: ["Répondre aux questions", "Passer directement à la suite", "Fermer la page"], correct: 0 }];

const shuffle = a => { for (let i = a.length - 1; i > 0; i--) { const j = Math.floor(Math.random() * (i + 1)); [a[i], a[j]] = [a[j], a[i]]; } return a; };
const FEATS = ['ability', 'gender', 'task', 'transcript'];

// Sélection équilibrée : priorité à l'équilibre intra-participant, puis aux vidéos les moins vues globalement
async function pick(e) {
  const vs = await q(`select v.*,(select count(*) from assignments a join sessions s on s.id=a.session_id where a.video_id=v.id and (s.completed or s.created_at>now()-interval '2 hours'))::int n from videos v where experiment_id=$1`, [e.id]);
  let pool_ = vs.filter(v => !v.attention); const att = shuffle(vs.filter(v => v.attention)).slice(0, e.n_attention), out = [], c = {};
  while (out.length < e.n_videos && pool_.length) {
    let best = null, bs = Infinity;
    for (const v of pool_) {
      const s = 1000 * FEATS.reduce((t, f) => t + (c[f + ':' + v[f]] || 0), 0) + v.n + Math.random();
      if (s < bs) { bs = s; best = v; }
    }
    FEATS.forEach(f => c[f + ':' + best[f]] = (c[f + ':' + best[f]] || 0) + 1);
    out.push(best); pool_ = pool_.filter(v => v !== best);
  }
  shuffle(out); att.forEach(a => out.splice(1 + Math.floor(Math.random() * out.length), 0, a));
  return out;
}

// ---- API participants
app.get('/api/public', async (_, res) => res.json(await q('select id,name from experiments where active order by id')));
app.get('/api/public/:id', async (req, res) => {
  const [e] = await q('select id,name,instructions,comprehension from experiments where id=$1 and active', [req.params.id]);
  if (!e) return res.status(404).json({});
  e.comprehension = e.comprehension.map(c => ({ q: c.q, options: c.options })); res.json(e);
});
app.post('/api/start', async (req, res) => {
  const { experiment_id, audio_ok, attempts, comp = [] } = req.body;
  const [e] = await q('select * from experiments where id=$1 and active', [experiment_id]); if (!e) return res.status(404).json({});
  if (!e.comprehension.every((c, i) => comp[i] === c.correct)) return res.json({ ok: false });
  const vids = await pick(e), id = crypto.randomUUID();
  await q('insert into sessions(id,experiment_id,audio_ok,comp_attempts) values($1,$2,$3,$4)', [id, e.id, !!audio_ok, attempts || 1]);
  for (let i = 0; i < vids.length; i++) await q('insert into assignments values($1,$2,$3)', [id, vids[i].id, i]);
  const qs = await q('select id,kind,text,left_label,right_label,scale from questions where experiment_id=$1 order by pos', [e.id]);
  res.json({ ok: true, session: id, questions: qs, videos: vids.map(v => ({ id: v.id, yt: v.yt, instruction: v.instruction })) });
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
app.post('/api/finish', async (req, res) => { await q('update sessions set completed=true where id=$1', [req.body.session]); res.json({ ok: true }); });

// ---- API admin
const A = '/api/admin';
app.get(A + '/experiments', async (_, res) => res.json(await q('select * from experiments order by id')));
app.post(A + '/experiments', async (req, res) => res.json((await q('insert into experiments(name,instructions,comprehension) values($1,$2,$3) returning *', [req.body.name || 'Nouvelle expérience', DEF_INSTR, JSON.stringify(DEF_COMP)]))[0]));
app.put(A + '/experiments/:id', async (req, res) => {
  const b = req.body;
  res.json((await q('update experiments set name=$1,active=$2,n_videos=$3,n_attention=$4,instructions=$5,comprehension=$6 where id=$7 returning *', [b.name, b.active, b.n_videos, b.n_attention, b.instructions, JSON.stringify(b.comprehension), req.params.id]))[0]);
});
app.delete(A + '/experiments/:id', async (req, res) => { await q('delete from sessions where experiment_id=$1', [req.params.id]); await q('delete from experiments where id=$1', [req.params.id]); res.json({ ok: true }); });
app.get(A + '/experiments/:id', async (req, res) => {
  const id = req.params.id;
  res.json({
    questions: await q('select * from questions where experiment_id=$1 order by pos', [id]),
    videos: await q(`select v.*,(select count(*) from assignments a where a.video_id=v.id)::int assigned,(select count(*) from responses r where r.video_id=v.id)::int answered,(select count(*) from responses r where r.video_id=v.id and r.attention_passed)::int att_ok from videos v where experiment_id=$1 order by id`, [id]),
    sessions: (await q('select count(*)::int total,count(*) filter(where completed)::int done from sessions where experiment_id=$1', [id]))[0]
  });
});
app.post(A + '/experiments/:id/questions', async (req, res) => {
  const b = req.body; await q('insert into questions(experiment_id,kind,text,left_label,right_label,scale) values($1,$2,$3,$4,$5,$6)', [req.params.id, b.kind, b.text, b.left_label || '', b.right_label || '', b.scale || 7]); res.json({ ok: true });
});
app.delete(A + '/questions/:id', async (req, res) => { await q('delete from questions where id=$1', [req.params.id]); res.json({ ok: true }); });
app.delete(A + '/videos/:id', async (req, res) => { await q('delete from videos where id=$1', [req.params.id]); res.json({ ok: true }); });
// Format : url;capacité;genre;tâche;transcript[;réponse attendue Likert (=vidéo piège);consigne]
app.post(A + '/experiments/:id/videos', async (req, res) => {
  let n = 0; const bad = [];
  for (const l of String(req.body.text).split('\n').map(x => x.trim()).filter(Boolean)) {
    const p = l.split(';').map(x => x.trim());
    const m = p[0].match(/(?:v=|youtu\.be\/|embed\/|shorts\/)([\w-]{11})/) || p[0].match(/^([\w-]{11})$/);
    if (!m || p.length < 5) { bad.push(l); continue; }
    const ex = parseInt(p[5]), isAtt = !isNaN(ex);
    await q('insert into videos(experiment_id,yt,ability,gender,task,transcript,attention,expected,instruction) values($1,$2,$3,$4,$5,$6,$7,$8,$9)', [req.params.id, m[1], p[1], p[2], p[3], p[4], isAtt, isAtt ? ex : null, p.slice(6).join(';')]);
    n++;
  }
  res.json({ added: n, bad });
});
app.get(A + '/experiments/:id/export.csv', async (req, res) => {
  const id = req.params.id, qs = await q('select id from questions where experiment_id=$1 order by pos', [id]);
  const rows = await q('select r.*,v.yt,v.ability,v.gender,v.task,v.transcript,v.attention,s.audio_ok,s.comp_attempts,s.completed,a.pos from responses r join videos v on v.id=r.video_id join sessions s on s.id=r.session_id join assignments a on a.session_id=r.session_id and a.video_id=r.video_id where v.experiment_id=$1 order by r.session_id,a.pos', [id]);
  const cols = ['session_id', 'pos', 'yt', 'ability', 'gender', 'task', 'transcript', 'attention', 'attention_passed', 'ended', 'audio_ok', 'comp_attempts', 'completed'];
  const esc = x => '"' + String(x ?? '').replace(/"/g, '""') + '"';
  res.type('text/csv').send([[...cols, ...qs.map(x => 'q' + x.id)].join(','), ...rows.map(r => [...cols.map(c => r[c]), ...qs.map(x => r.answers[x.id])].map(esc).join(','))].join('\n'));
});

pool.query(SCHEMA).then(() => app.listen(process.env.PORT || 3000, () => console.log('OK'))).catch(e => { console.error(e); process.exit(1); });
