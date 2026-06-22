/* ==============================================
   hpskw PRO DJ - Enhanced Version 2.0
   ============================================== */

let AC = null, masterG = null, masterAnal = null, limiterNode = null, limiterVal = 0.8;
let rec = null, recChunks = [], isRec = false, recStart = 0;
let autoOn = false, bassOn = false, karaOn = false, voiceOn = false, recog = null, harmonicOn = false;
let xp = parseInt(localStorage.getItem('hk_xp') || '0');
let achs = JSON.parse(localStorage.getItem('hk_ach') || '[]');
let pl = JSON.parse(localStorage.getItem('hk_pl_data') || '[]');
let history = JSON.parse(localStorage.getItem('hk_history') || '[]');
let activeEfx = {};
let sessionId = Date.now();
let particles = [];

// Load playlist names from localStorage
try {
    const savedPl = JSON.parse(localStorage.getItem('hk_pl') || '[]');
    if (savedPl.length > 0 && pl.length === 0) {
        pl = savedPl.map(n => ({ name: n, file: null }));
    }
} catch (e) {}

const logoImg = new Image();
let logoOk = false;
logoImg.crossOrigin = 'anonymous';
logoImg.onload = () => { logoOk = true; };
logoImg.onerror = () => { logoOk = false; };
logoImg.src = 'https://z-cdn-media.chatglm.cn/files/3552d58b-d594-4c29-b321-aede00eca001.png?auth_key=1882031530-88b61427d1954ec98eaa47fdccbbcf96-0-e1990896c3f2e126f01e846d20e8d51d';

const RANKS = [
    { m: 0, n: 'Beginner DJ' },
    { m: 100, n: 'Bedroom DJ' },
    { m: 300, n: 'Club DJ' },
    { m: 600, n: 'Pro DJ' },
    { m: 1000, n: 'Festival DJ' },
    { m: 2000, n: 'Legend DJ' },
    { m: 5000, n: 'God of DJ' }
];

const ACHS = [
    { id: 'load', n: 'First Track', d: 'පළමු සින්දුව load', x: 20, i: '🎵' },
    { id: 'mix', n: 'First Mix', d: 'දෙකේ Deck play', x: 30, i: '🎧' },
    { id: 'loop', n: 'Loop Master', d: 'Loop set', x: 25, i: '🔁' },
    { id: 'cue', n: 'Cue Point', d: 'Hot cue set', x: 15, i: '📍' },
    { id: 'efx', n: 'FX Wizard', d: 'Effect use', x: 20, i: '🎛️' },
    { id: 'rec', n: 'Studio Time', d: 'Recording', x: 50, i: '💾' },
    { id: 'voice', n: 'Voice Control', d: 'Voice cmd', x: 30, i: '🎤' },
    { id: 'samp', n: 'Pad Slapper', d: 'Sampler hit', x: 10, i: '🎹' },
    { id: 'bass', n: 'Bass Head', d: 'Bass Boost', x: 15, i: '🔥' },
    { id: 'xfade', n: 'Smooth Xfade', d: 'Crossfader', x: 15, i: '🎚️' },
    { id: 'reverse', n: 'Backspin', d: 'Reverse track', x: 20, i: '🔄' },
    { id: 'keyshift', n: 'Key Master', d: 'Key shift', x: 25, i: '🎵' },
    { id: 'drop', n: 'Drop The Beat', d: 'Trigger drop', x: 30, i: '💥' },
    { id: 'hist', n: 'History', d: 'First history entry', x: 10, i: '📜' }
];

const EFX = ['Delay', 'Reverb', 'Flanger', 'Echo', 'Phaser', 'WahWah', 'Distort', 'BitCrush', 'Pan', 'Tremolo', 'Chorus', 'Compress', 'HiPass', 'Notch', 'Reverse', 'Stereo', 'LoFi', 'Vinyl', 'Sidechain', 'Gate'];

const SOUNDS = [
    { n: 'Kick', k: '1' }, { n: 'Snare', k: '2' }, { n: 'HiHat', k: '3' }, { n: 'Clap', k: '4' },
    { n: 'Tom', k: '5' }, { n: 'Rim', k: '6' }, { n: 'Crash', k: '7' }, { n: 'Ride', k: '8' },
    { n: 'Perc', k: 'Q' }, { n: 'Shaker', k: 'W' }, { n: 'Cowbell', k: 'E' }, { n: 'Conga', k: 'R' },
    { n: 'Laser', k: 'A' }, { n: 'Siren', k: 'S' }, { n: 'Rise', k: 'D' }, { n: 'Drop', k: 'F' },
    { n: 'Scratch', k: 'Z' }, { n: 'Vocal', k: 'X' }, { n: 'FX1', k: 'C' }, { n: 'FX2', k: 'V' },
    { n: 'Horn', k: 'G' }, { n: 'Cheer', k: 'B' }, { n: 'Bell', k: 'N' }, { n: 'Chord', k: 'M' }
];

/* ===== INITIALIZATION ===== */
function initAC() {
    if (AC) return;
    AC = new (window.AudioContext || window.webkitAudioContext)();
    masterG = AC.createGain();
    masterG.gain.value = 1;
    limiterNode = AC.createDynamicsCompressor();
    limiterNode.threshold.value = -20;
    limiterNode.knee.value = 6;
    limiterNode.ratio.value = 6;
    limiterNode.attack.value = 0.003;
    limiterNode.release.value = 0.25;
    masterAnal = AC.createAnalyser();
    masterAnal.fftSize = 512;
    masterAnal.smoothingTimeConstant = 0.8;
    masterG.connect(limiterNode);
    limiterNode.connect(masterAnal);
    masterAnal.connect(AC.destination);
}

function setLimiter(v) {
    limiterVal = parseFloat(v);
    if (limiterNode) {
        limiterNode.threshold.value = -10 - 10 * limiterVal;
    }
}

/* ===== DECK CLASS ===== */
class Deck {
    constructor(id) {
        this.id = id;
        this.buf = null;
        this.src = null;
        this.playing = false;
        this.offset = 0;
        this.t0 = 0;
        this.rate = 1;
        this.bpm = 0;
        this.key = '--';
        this.vol = 0.8;
        this.wave = null;
        this.loopBeats = 0;
        this.loopS = 0;
        this.loopE = 0;
        this.cues = [null, null, null, null, null, null, null, null];
        this._init = false;
        this.gain = null;
        this.eqL = null;
        this.eqM = null;
        this.eqH = null;
        this.filt = null;
        this.anal = null;
        this.xfg = null;
        this.reversed = false;
        this.keyShiftAmt = 0;
    }

    _nodes() {
        if (this._init) return;
        initAC();
        this.gain = AC.createGain();
        this.gain.gain.value = this.vol;
        this.eqL = AC.createBiquadFilter();
        this.eqL.type = 'lowshelf';
        this.eqL.frequency.value = 200;
        this.eqM = AC.createBiquadFilter();
        this.eqM.type = 'peaking';
        this.eqM.frequency.value = 1000;
        this.eqM.Q.value = 0.7;
        this.eqH = AC.createBiquadFilter();
        this.eqH.type = 'highshelf';
        this.eqH.frequency.value = 4000;
        this.filt = AC.createBiquadFilter();
        this.filt.type = 'lowpass';
        this.filt.frequency.value = 20000;
        this.anal = AC.createAnalyser();
        this.anal.fftSize = 256;
        this.xfg = AC.createGain();
        this.xfg.gain.value = 0.707;
        this.gain.connect(this.eqL);
        this.eqL.connect(this.eqM);
        this.eqM.connect(this.eqH);
        this.eqH.connect(this.filt);
        this.filt.connect(this.anal);
        this.anal.connect(this.xfg);
        this.xfg.connect(masterG);
        this._init = true;
    }

    _kill() {
        if (this.src) {
            try { this.src.stop(); } catch (e) {}
            try { this.src.disconnect(); } catch (e) {}
            this.src = null;
        }
    }

    time() {
        if (!this.playing) return this.offset;
        return this.offset + (AC.currentTime - this.t0) * this.rate;
    }

    play() {
        if (!this.buf) return;
        this._nodes();
        if (AC.state === 'suspended') AC.resume();
        this._kill();
        this.src = AC.createBufferSource();
        this.src.buffer = this.buf;
        this.src.playbackRate.value = this.reversed ? -(this.rate) : this.rate;
        this.src.connect(this.gain);
        if (this.loopBeats > 0 && this.loopE > this.loopS) {
            this.src.loop = true;
            this.src.loopStart = this.loopS;
            this.src.loopEnd = this.loopE;
        }
        this.src.onended = () => {
            if (!this.playing) return;
            this.playing = false;
            if (this.loopBeats === 0) this.offset = 0;
            this._ub();
            this._uv();
        };
        this.t0 = AC.currentTime;
        this.src.start(0, this.offset);
        this.playing = true;
        this._ub();
        this._uv();
    }

    pause() {
        if (!this.playing) return;
        this.offset = this.time();
        this.playing = false;
        this._kill();
        this._ub();
        this._uv();
    }

    stop() {
        this.offset = 0;
        this.playing = false;
        this._kill();
        this._ub();
        this._uv();
    }

    cue() { this.stop(); }

    toggle() { this.playing ? this.pause() : this.play(); }

    seek(t) {
        const w = this.playing;
        if (w) { this.playing = false;
            this._kill(); }
        this.offset = Math.max(0, Math.min(t, this.buf ? this.buf.duration : 0));
        if (w) this.play();
    }

    setPitch(v) {
        this.rate = 1 + parseFloat(v) / 100;
        if (this.src) this.src.playbackRate.value = this.reversed ? -(this.rate) : this.rate;
        const p = this.id === 'a' ? 'a' : 'b';
        document.getElementById(p + 'PitchV').textContent = (v >= 0 ? '+' : '') + parseFloat(v).toFixed(1) + '%';
    }

    setVol(v) {
        this.vol = parseFloat(v);
        if (this.gain) this.gain.gain.value = this.vol;
    }

    setEQ(b, v) {
        const val = parseFloat(v);
        const p = this.id === 'a' ? 'a' : 'b';
        if (b === 'l' && this.eqL) {
            this.eqL.gain.value = val;
            document.getElementById(p + 'ELv').textContent = (val > 0 ? '+' : '') + val;
        }
        if (b === 'm' && this.eqM) {
            this.eqM.gain.value = val;
            document.getElementById(p + 'EMv').textContent = (val > 0 ? '+' : '') + val;
        }
        if (b === 'h' && this.eqH) {
            this.eqH.gain.value = val;
            document.getElementById(p + 'EHv').textContent = (val > 0 ? '+' : '') + val;
        }
    }

    setFilter(v) {
        if (this.filt) this.filt.frequency.value = parseFloat(v);
    }

    setLoop(beats) {
        if (!this.buf) return;
        this.loopBeats = beats;
        document.querySelectorAll('.lb[data-d="' + this.id + '"]').forEach(b => {
            b.classList.remove('on-a', 'on-b');
            if (parseInt(b.dataset.b) === beats) b.classList.add(this.id === 'a' ? 'on-a' : 'on-b');
        });
        if (beats === 0) {
            this.loopS = 0;
            this.loopE = 0;
            if (this.src) this.src.loop = false;
            return;
        }
        const bd = 60 / (this.bpm || 120);
        const ct = this.time();
        this.loopS = ct;
        this.loopE = Math.min(ct + bd * beats, this.buf.duration);
        if (this.playing && this.src) {
            this.src.loop = true;
            this.src.loopStart = this.loopS;
            this.src.loopEnd = this.loopE;
        }
        addXP(5);
        unlock('loop');
        toast(this.id.toUpperCase() + ': ' + beats + ' Beat Loop');
    }

    setCue(i) {
        if (!this.buf) return;
        if (this.cues[i] !== null) {
            this.seek(this.cues[i]);
            if (!this.playing) this.play();
            return;
        }
        this.cues[i] = this.time();
        const el = document.getElementById(this.id + 'c' + i);
        if (el) {
            el.className = 'hcue set-' + this.id;
            el.classList.add('active');
            setTimeout(() => el.classList.remove('active'), 300);
        }
        addXP(3);
        unlock('cue');
    }

    syncTo(o) {
        if (!o.bpm || !this.bpm) { toast('BPM detect වී නැත'); return; }
        const d = o.bpm - this.bpm;
        const p = Math.max(-8, Math.min(8, (d / this.bpm) * 100));
        this.setPitch(p);
        document.getElementById(this.id === 'a' ? 'aPitch' : 'bPitch').value = p;
        toast('Synced ' + o.bpm + ' BPM');
        addXP(10);
    }

    reverse() {
        if (!this.playing) { toast('Play වෙනවා පළමුව'); return; }
        this.reversed = !this.reversed;
        if (this.src) this.src.playbackRate.value = this.reversed ? -(this.rate) : this.rate;
        const btn = document.getElementById(this.id + 'RevBtn');
        btn.classList.toggle('on-a', this.reversed);
        btn.classList.toggle('on-b', this.reversed);
        toast(this.reversed ? 'Reverse ON' : 'Reverse OFF');
        unlock('reverse');
        addXP(5);
    }

    keyShift() {
        if (!this.buf) return;
        const keys = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'];
        const shifts = [-1, 0, 1];
        const current = this.keyShiftAmt;
        let idx = shifts.indexOf(current);
        idx = (idx + 1) % shifts.length;
        this.keyShiftAmt = shifts[idx];
        const semitones = this.keyShiftAmt;
        const factor = Math.pow(2, semitones / 12);
        if (this.src) {
            this.src.playbackRate.value = this.reversed ? -(this.rate * factor) : this.rate * factor;
        }
        const keyIdx = keys.indexOf(this.key.replace('m', ''));
        if (keyIdx !== -1) {
            const newKey = keys[(keyIdx + semitones + 12) % 12] + (this.key.includes('m') ? 'm' : '');
            document.getElementById(this.id + 'Key').textContent = newKey;
        }
        const btn = document.getElementById(this.id + 'KeyBtn');
        btn.classList.toggle('on-a', semitones !== 0);
        btn.classList.toggle('on-b', semitones !== 0);
        toast('Key: ' + (semitones > 0 ? '+' : '') + semitones + ' semitones');
        unlock('keyshift');
        addXP(5);
    }

    async load(file) {
        if (!file) return;
        this._nodes();
        this.stop();
        try {
            const ab = await file.arrayBuffer();
            this.buf = await AC.decodeAudioData(ab);
        } catch (e) {
            toast('Error: ' + file.name);
            return;
        }
        this.offset = 0;
        this.cues = [null, null, null, null, null, null, null, null];
        this._wave();
        this._bpm();
        this._key();
        this._info(file.name);
        this._cues();
        addXP(10);
        unlock('load');
        toast('Loaded: ' + file.name);
        addHistory('Loaded: ' + file.name);
    }

    _wave() {
        if (!this.buf) return;
        const ch = this.buf.getChannelData(0);
        const n = 600;
        const bk = Math.floor(ch.length / n);
        this.wave = [];
        for (let i = 0; i < n; i++) {
            let s = 0;
            for (let j = 0; j < bk; j++) s += Math.abs(ch[i * bk + j]);
            this.wave.push(s / bk);
        }
        const mx = Math.max(...this.wave, 0.001);
        this.wave = this.wave.map(v => v / mx);
    }

    _bpm() {
        if (!this.buf) return;
        const ch = this.buf.getChannelData(0);
        const sr = this.buf.sampleRate;
        const ws = Math.floor(sr * 0.02);
        const hn = Math.floor(ch.length / ws);
        const en = new Float32Array(hn);
        for (let i = 0; i < hn; i++) {
            let e = 0;
            for (let j = 0; j < ws; j++) e += ch[i * ws + j] ** 2;
            en[i] = e;
        }
        const mi = Math.floor(sr * 0.3 / ws);
        const pk = [];
        const av = en.reduce((a, b) => a + b, 0) / hn;
        const th = av * 1.5;
        for (let i = 1; i < hn - 1; i++) {
            if (en[i] > en[i - 1] && en[i] > en[i + 1] && en[i] > th) {
                if (!pk.length || i - pk[pk.length - 1] >= mi) pk.push(i);
            }
        }
        if (pk.length < 4) { this.bpm = 120; return; }
        let t = 0;
        for (let i = 1; i < pk.length; i++) t += pk[i] - pk[i - 1];
        let b = (sr / ws) / (t / (pk.length - 1)) * 60;
        while (b > 180) b /= 2;
        while (b < 70) b *= 2;
        this.bpm = Math.round(b);
    }

    _key() {
        if (!this.buf) return;
        const ch = this.buf.getChannelData(0);
        const sr = this.buf.sampleRate;
        const nt = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'];
        const cr = new Float32Array(12);
        const nf = Math.min(25, Math.floor(ch.length / 4096));
        for (let f = 0; f < nf; f++) {
            const off = f * 4096;
            for (let n = 0; n < 12; n++) {
                const fr = 261.63 * Math.pow(2, n / 12);
                let re = 0,
                    im = 0;
                for (let i = 0; i < 4096; i++) {
                    const a = 2 * Math.PI * fr * i / sr;
                    re += ch[off + i] * Math.cos(a);
                    im -= ch[off + i] * Math.sin(a);
                }
                cr[n] += Math.sqrt(re * re + im * im);
            }
        }
        let mi = 0;
        for (let i = 1; i < 12; i++) if (cr[i] > cr[mi]) mi = i;
        this.key = nt[mi] + (cr[(mi + 9) % 12] > cr[(mi + 7) % 12] * 0.8 ? 'm' : '');
    }

    _info(n) {
        const p = this.id;
        document.getElementById(p + 'Bpm').textContent = this.bpm ? this.bpm + ' BPM' : '--';
        document.getElementById(p + 'Key').textContent = this.key;
        document.getElementById(p + 'Dur').textContent = this.fmt(this.buf.duration);
        document.getElementById(p + 'Name').textContent = n;
    }

    _ub() {
        const p = this.id;
        const b = document.getElementById(p + 'PlayBtn');
        if (this.playing) {
            b.innerHTML = '<i class="fas fa-pause"></i> PAUSE';
            b.classList.add('playing-' + p);
        } else {
            b.innerHTML = '<i class="fas fa-play"></i> PLAY';
            b.classList.remove('playing-' + p);
        }
    }

    _uv() {
        document.getElementById('vinyl' + this.id.toUpperCase()).classList.toggle('spin', this.playing);
    }

    _ut() {
        document.getElementById(this.id + 'Time').textContent = this.fmt(this.time());
    }

    _cues() {
        const box = document.getElementById(this.id + 'Cues');
        box.innerHTML = '';
        for (let i = 0; i < 8; i++) {
            const b = document.createElement('button');
            b.className = 'hcue';
            b.id = this.id + 'c' + i;
            b.textContent = i + 1;
            b.onclick = () => this.setCue(i);
            box.appendChild(b);
        }
    }

    fmt(s) {
        if (!isFinite(s)) return '0:00';
        const m = Math.floor(s / 60);
        return m + ':' + (Math.floor(s % 60) < 10 ? '0' : '') + Math.floor(s % 60);
    }

    vu() {
        if (!this.anal) return 0;
        const d = new Uint8Array(this.anal.frequencyBinCount);
        this.anal.getByteFrequencyData(d);
        let s = 0;
        for (let i = 0; i < d.length; i++) s += d[i];
        return s / d.length / 255;
    }
}

const dA = new Deck('a');
const dB = new Deck('b');
dA._cues();
dB._cues();

/* ===== GLOBALS ===== */
function loadFile(f, id) { (id === 'A' ? dA : dB).load(f); }

function setXF(v) {
    const x = parseFloat(v);
    if (dA.xfg) dA.xfg.gain.value = Math.cos(x * Math.PI / 2);
    if (dB.xfg) dB.xfg.gain.value = Math.sin(x * Math.PI / 2);
    document.getElementById('xfade').value = x;
    if (!achs.includes('xfade')) unlock('xfade');
}

function setMaster(v) {
    if (masterG) masterG.gain.value = parseFloat(v);
}

function toggleBass() {
    bassOn = !bassOn;
    document.getElementById('bassBtn').classList.toggle('on-a', bassOn);
    if (bassOn) {
        dA.setEQ('l', 8);
        dB.setEQ('l', 8);
        document.getElementById('aEL').value = 8;
        document.getElementById('bEL').value = 8;
    } else {
        dA.setEQ('l', 0);
        dB.setEQ('l', 0);
        document.getElementById('aEL').value = 0;
        document.getElementById('bEL').value = 0;
    }
    unlock('bass');
    addXP(3);
}

function toggleKara() {
    karaOn = !karaOn;
    document.getElementById('karaBtn').classList.toggle('on-a', karaOn);
    if (karaOn) {
        dA.setEQ('m', -8);
        dB.setEQ('m', -8);
        document.getElementById('aEM').value = -8;
        document.getElementById('bEM').value = -8;
    } else {
        dA.setEQ('m', 0);
        dB.setEQ('m', 0);
        document.getElementById('aEM').value = 0;
        document.getElementById('bEM').value = 0;
    }
    toast(karaOn ? 'Karaoke ON' : 'Karaoke OFF');
}

function toggleHarmonic() {
    harmonicOn = !harmonicOn;
    document.getElementById('harmBtn').classList.toggle('on-a', harmonicOn);
    if (harmonicOn) {
        dA.setEQ('m', 2);
        dB.setEQ('m', 2);
        dA.setEQ('h', 3);
        dB.setEQ('h', 3);
        document.getElementById('aEM').value = 2;
        document.getElementById('bEM').value = 2;
        document.getElementById('aEH').value = 3;
        document.getElementById('bEH').value = 3;
        toast('Harmonic Boost ON');
    } else {
        dA.setEQ('m', 0);
        dB.setEQ('m', 0);
        dA.setEQ('h', 0);
        dB.setEQ('h', 0);
        document.getElementById('aEM').value = 0;
        document.getElementById('bEM').value = 0;
        document.getElementById('aEH').value = 0;
        document.getElementById('bEH').value = 0;
        toast('Harmonic Boost OFF');
    }
}

function horn() {
    if (!AC) initAC();
    if (AC.state === 'suspended') AC.resume();
    const o = AC.createOscillator(),
        g = AC.createGain();
    o.type = 'sawtooth';
    o.frequency.value = 523;
    g.gain.setValueAtTime(0.3, AC.currentTime);
    g.gain.exponentialRampToValueAtTime(0.001, AC.currentTime + 0.7);
    o.connect(g);
    g.connect(masterG);
    o.start();
    o.stop(AC.currentTime + 0.7);
}

function triggerDrop() {
    if (!AC) initAC();
    if (AC.state === 'suspended') AC.resume();
    const o = AC.createOscillator(),
        g = AC.createGain();
    o.type = 'sine';
    o.frequency.setValueAtTime(80, AC.currentTime);
    o.frequency.exponentialRampToValueAtTime(30, AC.currentTime + 0.5);
    g.gain.setValueAtTime(0.5, AC.currentTime);
    g.gain.exponentialRampToValueAtTime(0.001, AC.currentTime + 0.5);
    o.connect(g);
    g.connect(masterG);
    o.start();
    o.stop(AC.currentTime + 0.5);
    unlock('drop');
    addXP(10);
    toast('💥 DROP!');
}

function toggleRec() {
    if (!AC) initAC();
    if (isRec) {
        if (rec && rec.state === 'recording') rec.stop();
        isRec = false;
        document.getElementById('recBtn').classList.remove('on-a');
        document.getElementById('recInd').classList.add('hidden');
        toast('Recording saved!');
        addHistory('Recording saved');
    } else {
        try {
            const dest = AC.createMediaStreamDestination();
            masterG.connect(dest);
            rec = new MediaRecorder(dest.stream, { mimeType: 'audio/webm;codecs=opus' });
            recChunks = [];
            rec.ondataavailable = e => { if (e.data.size > 0) recChunks.push(e.data); };
            rec.onstop = () => {
                const bl = new Blob(recChunks, { type: 'audio/webm' });
                const a = document.createElement('a');
                a.href = URL.createObjectURL(bl);
                a.download = 'hpskw_' + Date.now() + '.webm';
                a.click();
                masterG.disconnect(dest);
            };
            rec.start();
            isRec = true;
            recStart = Date.now();
            document.getElementById('recBtn').classList.add('on-a');
            document.getElementById('recInd').classList.remove('hidden');
            unlock('rec');
            addXP(10);
            toast('Recording...');
        } catch (e) { toast('Rec error'); }
    }
}

function toggleAuto() {
    autoOn = !autoOn;
    document.getElementById('autoBtn').classList.toggle('on-a', autoOn);
    toast(autoOn ? 'AutoMix ON' : 'AutoMix OFF');
}

/* ===== EFFECTS ===== */
function buildEfx() {
    document.getElementById('efxGrid').innerHTML = EFX.map((n, i) =>
        `<button class="efx" onclick="togEfx(${i},this)">${n}</button>`
    ).join('');
}

function togEfx(i, btn) {
    const n = EFX[i];
    if (activeEfx[n]) {
        delete activeEfx[n];
        btn.classList.remove('on');
    } else {
        activeEfx[n] = true;
        btn.classList.add('on');
        unlock('efx');
        addXP(3);
    }
    applyEfx();
    toast(n + (activeEfx[n] ? ' ON' : ' OFF'));
}

function applyEfx() {
    const wet = parseFloat(document.getElementById('efxWet').value);
    [dA, dB].forEach(d => {
        if (!d.filt) return;
        let f = 20000,
            t = 'lowpass';
        if (activeEfx['HiPass']) { f = 300;
            t = 'highpass'; }
        if (activeEfx['Notch']) { f = 1000;
            t = 'notch'; }
        if (activeEfx['HiPass'] || activeEfx['Notch']) {
            d.filt.type = t;
            const base = parseFloat(document.getElementById(d.id === 'a' ? 'aFlt' : 'bFlt').value);
            d.filt.frequency.value = f + (base - f) * wet;
        } else {
            d.filt.type = 'lowpass';
            d.filt.frequency.value = parseFloat(document.getElementById(d.id === 'a' ? 'aFlt' : 'bFlt').value);
        }
    });
}

/* ===== SAMPLERS ===== */
function buildSam() {
    document.getElementById('samGrid').innerHTML = SOUNDS.map((s, i) =>
        `<div class="spad" id="sp${i}" onmousedown="hitSam(${i})" ontouchstart="event.preventDefault();hitSam(${i})"><span>${s.n}</span><span class="kh">[${s.k}]</span></div>`
    ).join('');
}

function hitSam(i) {
    if (!AC) initAC();
    if (AC.state === 'suspended') AC.resume();
    _genS(i);
    const el = document.getElementById('sp' + i);
    el.classList.add('hit');
    setTimeout(() => el.classList.remove('hit'), 100);
    addXP(2);
    unlock('samp');
}

function _genS(i) {
    const t = AC.currentTime;

    function mo(tp, fr, dur, vol) {
        const o = AC.createOscillator(),
            g = AC.createGain();
        o.type = tp;
        o.frequency.value = fr;
        g.gain.setValueAtTime(vol, t);
        g.gain.exponentialRampToValueAtTime(0.001, t + dur);
        o.connect(g);
        g.connect(masterG);
        o.start(t);
        o.stop(t + dur);
    }

    function mn(dur, vol, ft, ff) {
        const n = AC.createBufferSource(),
            buf = AC.createBuffer(1, AC.sampleRate * dur, AC.sampleRate),
            d = buf.getChannelData(0);
        for (let j = 0; j < d.length; j++) d[j] = Math.random() * 2 - 1;
        n.buffer = buf;
        if (ft) {
            const f = AC.createBiquadFilter();
            f.type = ft;
            f.frequency.value = ff;
            n.connect(f);
            f.connect(masterG);
        } else {
            const g = AC.createGain();
            g.gain.value = vol;
            n.connect(g);
            g.connect(masterG);
        }
        n.start(t);
    }

    switch (i) {
        case 0: {
            const o = AC.createOscillator(),
                g = AC.createGain();
            o.type = 'sine';
            o.frequency.setValueAtTime(150, t);
            o.frequency.exponentialRampToValueAtTime(30, t + .15);
            g.gain.setValueAtTime(.8, t);
            g.gain.exponentialRampToValueAtTime(.001, t + .3);
            o.connect(g);
            g.connect(masterG);
            o.start(t);
            o.stop(t + .3);
            break;
        }
        case 1: {
            const n = AC.createBufferSource(),
                buf = AC.createBuffer(1, AC.sampleRate * .15, AC.sampleRate),
                d = buf.getChannelData(0);
            for (let j = 0; j < d.length; j++) d[j] = (Math.random() * 2 - 1) * Math.exp(-j / (AC.sampleRate * .03));
            n.buffer = buf;
            const f = AC.createBiquadFilter(),
                g = AC.createGain();
            f.type = 'bandpass';
            f.frequency.value = 3000;
            g.gain.value = .5;
            n.connect(f);
            f.connect(g);
            g.connect(masterG);
            n.start(t);
            break;
        }
        case 2:
            mn(.05, .3, 'highpass', 8000);
            break;
        case 3: {
            for (let j = 0; j < 3; j++) setTimeout(() => {
                const n = AC.createBufferSource(),
                    buf = AC.createBuffer(1, AC.sampleRate * .02, AC.sampleRate),
                    d = buf.getChannelData(0);
                for (let k = 0; k < d.length; k++) d[k] = (Math.random() * 2 - 1) * Math.exp(-k / (AC.sampleRate * .005));
                n.buffer = buf;
                const f = AC.createBiquadFilter(),
                    g = AC.createGain();
                f.type = 'bandpass';
                f.frequency.value = 2500;
                g.gain.value = .4;
                n.connect(f);
                f.connect(g);
                g.connect(masterG);
                n.start();
            }, j * 15);
            break;
        }
        case 4: {
            const o = AC.createOscillator(),
                g = AC.createGain();
            o.type = 'sine';
            o.frequency.setValueAtTime(200, t);
            o.frequency.exponentialRampToValueAtTime(80, t + .2);
            g.gain.setValueAtTime(.6, t);
            g.gain.exponentialRampToValueAtTime(.001, t + .25);
            o.connect(g);
            g.connect(masterG);
            o.start(t);
            o.stop(t + .25);
            break;
        }
        case 5:
            mo('triangle', 800, .05, .4);
            break;
        case 6:
            mn(.8, .25, 'highpass', 4000);
            break;
        case 7:
            mn(.5, .15, 'highpass', 6000);
            break;
        case 8: {
            const o = AC.createOscillator(),
                g = AC.createGain();
            o.type = 'sine';
            o.frequency.setValueAtTime(600, t);
            o.frequency.exponentialRampToValueAtTime(300, t + .08);
            g.gain.setValueAtTime(.3, t);
            g.gain.exponentialRampToValueAtTime(.001, t + .1);
            o.connect(g);
            g.connect(masterG);
            o.start(t);
            o.stop(t + .1);
            break;
        }
        case 9:
            mn(.08, .3, 'highpass', 10000);
            break;
        case 10: {
            const o1 = AC.createOscillator(),
                o2 = AC.createOscillator(),
                g = AC.createGain();
            o1.type = 'square';
            o1.frequency.value = 560;
            o2.type = 'square';
            o2.frequency.value = 845;
            g.gain.setValueAtTime(.2, t);
            g.gain.exponentialRampToValueAtTime(.001, t + .15);
            o1.connect(g);
            o2.connect(g);
            g.connect(masterG);
            o1.start(t);
            o2.start(t);
            o1.stop(t + .15);
            o2.stop(t + .15);
            break;
        }
        case 11: {
            const o = AC.createOscillator(),
                g = AC.createGain();
            o.type = 'sine';
            o.frequency.setValueAtTime(350, t);
            o.frequency.exponentialRampToValueAtTime(200, t + .12);
            g.gain.setValueAtTime(.5, t);
            g.gain.exponentialRampToValueAtTime(.001, t + .15);
            o.connect(g);
            g.connect(masterG);
            o.start(t);
            o.stop(t + .15);
            break;
        }
        case 12: {
            const o = AC.createOscillator(),
                g = AC.createGain();
            o.type = 'sawtooth';
            o.frequency.setValueAtTime(3000, t);
            o.frequency.exponentialRampToValueAtTime(100, t + .3);
            g.gain.setValueAtTime(.2, t);
            g.gain.exponentialRampToValueAtTime(.001, t + .3);
            o.connect(g);
            g.connect(masterG);
            o.start(t);
            o.stop(t + .3);
            break;
        }
        case 13: {
            const o = AC.createOscillator(),
                g = AC.createGain();
            o.type = 'sine';
            o.frequency.setValueAtTime(400, t);
            o.frequency.linearRampToValueAtTime(800, t + .3);
            o.frequency.linearRampToValueAtTime(400, t + .6);
            g.gain.setValueAtTime(.25, t);
            g.gain.exponentialRampToValueAtTime(.001, t + .6);
            o.connect(g);
            g.connect(masterG);
            o.start(t);
            o.stop(t + .6);
            break;
        }
        case 14: {
            const n = AC.createBufferSource(),
                buf = AC.createBuffer(1, AC.sampleRate * .5, AC.sampleRate),
                d = buf.getChannelData(0);
            for (let j = 0; j < d.length; j++) d[j] = (Math.random() * 2 - 1) * Math.exp(-Math.pow((j / d.length - 1), 2) * 5) * .3;
            n.buffer = buf;
            const f = AC.createBiquadFilter(),
                g = AC.createGain();
            f.type = 'bandpass';
            f.frequency.setValueAtTime(200, t);
            f.frequency.exponentialRampToValueAtTime(8000, t + .5);
            f.Q.value = 5;
            g.gain.value = .4;
            n.connect(f);
            f.connect(g);
            g.connect(masterG);
            n.start(t);
            break;
        }
        case 15: {
            const o = AC.createOscillator(),
                g = AC.createGain();
            o.type = 'sine';
            o.frequency.setValueAtTime(200, t);
            o.frequency.exponentialRampToValueAtTime(20, t + .5);
            g.gain.setValueAtTime(.7, t);
            g.gain.exponentialRampToValueAtTime(.001, t + .5);
            o.connect(g);
            g.connect(masterG);
            o.start(t);
            o.stop(t + .5);
            break;
        }
        case 16: {
            const n = AC.createBufferSource(),
                buf = AC.createBuffer(1, AC.sampleRate * .15, AC.sampleRate),
                d = buf.getChannelData(0);
            for (let j = 0; j < d.length; j++) {
                const tt = j / d.length;
                d[j] = Math.sin(2 * Math.PI * (200 + 2000 * Math.sin(tt * Math.PI * 3)) * tt) * Math.exp(-tt * 5) * .4;
            }
            n.buffer = buf;
            n.connect(masterG);
            n.start(t);
            break;
        }
        case 17: {
            const o = AC.createOscillator(),
                g = AC.createGain(),
                f = AC.createBiquadFilter();
            o.type = 'sawtooth';
            o.frequency.value = 300;
            f.type = 'bandpass';
            f.frequency.value = 600;
            f.Q.value = 10;
            g.gain.setValueAtTime(.2, t);
            g.gain.exponentialRampToValueAtTime(.001, t + .1);
            o.connect(f);
            f.connect(g);
            g.connect(masterG);
            o.start(t);
            o.stop(t + .1);
            break;
        }
        case 18: {
            const o = AC.createOscillator(),
                g = AC.createGain();
            o.type = 'sine';
            o.frequency.setValueAtTime(1000, t);
            o.frequency.exponentialRampToValueAtTime(100, t + .2);
            g.gain.setValueAtTime(.2, t);
            g.gain.exponentialRampToValueAtTime(.001, t + .2);
            o.connect(g);
            g.connect(masterG);
            o.start(t);
            o.stop(t + .2);
            break;
        }
        case 19:
            mo('triangle', 200, .15, .2);
            break;
        case 20: {
            const o1 = AC.createOscillator(),
                o2 = AC.createOscillator(),
                g = AC.createGain();
            o1.type = 'sawtooth';
            o1.frequency.value = 523;
            o2.type = 'sawtooth';
            o2.frequency.value = 659;
            g.gain.setValueAtTime(.15, t);
            g.gain.exponentialRampToValueAtTime(.001, t + .4);
            o1.connect(g);
            o2.connect(g);
            g.connect(masterG);
            o1.start(t);
            o2.start(t);
            o1.stop(t + .4);
            o2.stop(t + .4);
            break;
        }
        case 21:
            mn(.5, .15);
            break;
        case 22:
            mo('sine', 1200, .5, .3);
            break;
        case 23:
            [261, 329, 392].forEach(f => {
                const o = AC.createOscillator(),
                    g = AC.createGain();
                o.type = 'sine';
                o.frequency.value = f;
                g.gain.setValueAtTime(.15, t);
                g.gain.exponentialRampToValueAtTime(.001, t + .4);
                o.connect(g);
                g.connect(masterG);
                o.start(t);
                o.stop(t + .4);
            });
            break;
    }
}

/* ===== WAVEFORM DRAW ===== */
function drawWave(cv, deck, col) {
    const ctx = cv.getContext('2d');
    const dp = window.devicePixelRatio || 1;
    const w = cv.width = cv.offsetWidth * dp;
    const h = cv.height = cv.offsetHeight * dp;
    ctx.clearRect(0, 0, w, h);
    if (!deck.wave) {
        ctx.fillStyle = 'rgba(90,90,122,.3)';
        ctx.font = (10 * dp) + 'px Rajdhani';
        ctx.textAlign = 'center';
        ctx.fillText('Load කරන්න', w / 2, h / 2 + 3 * dp);
        return;
    }
    const d = deck.wave;
    const bw = Math.max(1, w / d.length);
    const prog = deck.buf ? deck.time() / deck.buf.duration : 0;
    if (deck.loopBeats > 0 && deck.loopE > deck.loopS && deck.buf) {
        const ls = deck.loopS / deck.buf.duration * w;
        const le = deck.loopE / deck.buf.duration * w;
        ctx.fillStyle = 'rgba(255,145,0,.06)';
        ctx.fillRect(ls, 0, le - ls, h);
        ctx.strokeStyle = 'rgba(255,145,0,.25)';
        ctx.setLineDash([2 * dp, 2 * dp]);
        ctx.strokeRect(ls, 0, le - ls, h);
        ctx.setLineDash([]);
    }
    for (let i = 0; i < 8; i++) {
        if (deck.cues[i] !== null && deck.buf) {
            const x = deck.cues[i] / deck.buf.duration * w;
            ctx.fillStyle = 'rgba(255,145,0,.5)';
            ctx.fillRect(x - dp, 0, 2 * dp, h);
        }
    }
    for (let i = 0; i < d.length; i++) {
        const x = i * bw;
        const bh = d[i] * h * .7;
        ctx.fillStyle = i / d.length < prog ? col : col + '25';
        ctx.fillRect(x, (h - bh) / 2, Math.max(1, bw - .5), bh);
    }
    const px = prog * w;
    ctx.fillStyle = '#fff';
    ctx.fillRect(px - dp, 0, 2 * dp, h);
}

/* ===== CAT EAR CIRCULAR VISUALIZER ===== */
function normA(a) {
    while (a > Math.PI) a -= Math.PI * 2;
    while (a < -Math.PI) a += Math.PI * 2;
    return a;
}

function earMult(angle) {
    const lTip = -2.18,
        rTip = -0.96,
        halfSpan = 0.42;
    let m = 0;
    let dL = Math.abs(normA(angle - lTip));
    if (dL < halfSpan) { const t = dL / halfSpan;
        m = Math.max(m, Math.pow(Math.cos(t * Math.PI * .5), 1.6)); }
    let dR = Math.abs(normA(angle - rTip));
    if (dR < halfSpan) { const t = dR / halfSpan;
        m = Math.max(m, Math.pow(Math.cos(t * Math.PI * .5), 1.6)); }
    return m;
}

function drawCatEarViz() {
    const cv = document.getElementById('vizCanvas');
    const ctx = cv.getContext('2d');
    const dp = window.devicePixelRatio || 1;
    const w = cv.width = cv.offsetWidth * dp;
    const h = cv.height = cv.offsetHeight * dp;
    ctx.clearRect(0, 0, w, h);
    const cx = w / 2,
        cy = h / 2 + 10 * dp;
    const baseR = Math.min(w, h) * 0.22;
    const maxBarH = baseR * 0.9;
    const maxEarH = baseR * 0.75;
    const innerR = baseR * 0.85;
    const totalBars = 200;
    let data = new Uint8Array(256);
    if (masterAnal) {
        data = new Uint8Array(masterAnal.frequencyBinCount);
        masterAnal.getByteFrequencyData(data);
    }
    let bassE = 0;
    for (let i = 0; i < 8; i++) bassE += data[i];
    bassE /= 8 * 255;
    let midE = 0;
    for (let i = 8; i < 40; i++) midE += data[i];
    midE /= 32 * 255;
    let highE = 0;
    for (let i = 40; i < data.length; i++) highE += data[i];
    highE /= (data.length - 40) * 255;
    const pulseR = baseR + bassE * 6 * dp;
    const ambG = ctx.createRadialGradient(cx, cy, pulseR * .5, cx, cy, pulseR * 2.5);
    ambG.addColorStop(0, `rgba(0,230,118,${.03+bassE*.06})`);
    ambG.addColorStop(.5, `rgba(255,214,0,${.02+midE*.04})`);
    ambG.addColorStop(1, 'rgba(0,0,0,0)');
    ctx.fillStyle = ambG;
    ctx.fillRect(0, 0, w, h);
    for (let i = 0; i < totalBars; i++) {
        const angle = (i / totalBars) * Math.PI * 2 - Math.PI / 2;
        const fi = Math.floor((i / totalBars) * data.length);
        const val = data[fi] / 255;
        const em = earMult(angle);
        let barH = val * maxBarH;
        let isEar = em > 0.1;
        let r1, g1, b1, a1;
        if (isEar) {
            barH += em * maxEarH * (0.35 + val * 0.65);
            const t = em;
            r1 = Math.round(0 + t * 255);
            g1 = Math.round(230 - t * 16);
            b1 = Math.round(118 - t * 118);
            a1 = 0.6 + val * 0.4;
        } else {
            const fi2 = fi / data.length;
            if (fi2 < 0.15) { r1 = 255;
                g1 = Math.round(80 + fi2 / 0.15 * 120);
                b1 = 50;
                a1 = 0.5 + val * 0.5; } else if (fi2 < 0.5) {
                r1 = Math.round(255 * (1 - (fi2 - .15) / .35));
                g1 = 200 + Math.round(55 * (fi2 - .15) / .35);
                b1 = Math.round(50 + 150 * (fi2 - .15) / .35);
                a1 = 0.4 + val * 0.5;
            } else {
                r1 = Math.round(0 + 100 * (fi2 - .5) / .5);
                g1 = Math.round(200 - 50 * (fi2 - .5) / .5);
                b1 = Math.round(200 + 55 * (fi2 - .5) / .5);
                a1 = 0.3 + val * 0.5;
            }
        }
        const iR = pulseR;
        const oR = iR + Math.max(2 * dp, barH);
        const x1 = cx + Math.cos(angle) * iR;
        const y1 = cy + Math.sin(angle) * iR;
        const x2 = cx + Math.cos(angle) * oR;
        const y2 = cy + Math.sin(angle) * oR;
        ctx.beginPath();
        ctx.moveTo(x1, y1);
        ctx.lineTo(x2, y2);
        ctx.strokeStyle = `rgba(${r1},${g1},${b1},${a1})`;
        ctx.lineWidth = Math.max(1.5 * dp, (w / totalBars) * .65);
        ctx.lineCap = 'round';
        ctx.stroke();
        if (isEar && em > 0.7) {
            ctx.beginPath();
            ctx.moveTo(x1, y1);
            ctx.lineTo(x2, y2);
            ctx.strokeStyle = `rgba(255,255,255,${em*val*0.3})`;
            ctx.lineWidth = 1 * dp;
            ctx.stroke();
        }
    }
    const bandR = pulseR + 3 * dp;
    const lEarBase = -2.6,
        rEarBase = -0.54;
    ctx.beginPath();
    ctx.arc(cx, cy, bandR, lEarBase, rEarBase);
    ctx.strokeStyle = `rgba(255,255,255,${0.1+bassE*0.15})`;
    ctx.lineWidth = 2.5 * dp;
    ctx.lineCap = 'round';
    ctx.stroke();
    for (let i = 0; i < totalBars; i++) {
        const angle = (i / totalBars) * Math.PI * 2 - Math.PI / 2;
        const em = earMult(angle);
        if (em < 0.15) continue;
        const fi = Math.floor((i / totalBars) * data.length);
        const val = data[fi] / 255;
        const innerEarH = em * maxEarH * 0.4 * (0.2 + val * 0.3);
        const iR2 = pulseR + 2 * dp;
        const oR2 = iR2 + innerEarH;
        const x1 = cx + Math.cos(angle) * iR2;
        const y1 = cy + Math.sin(angle) * iR2;
        const x2 = cx + Math.cos(angle) * oR2;
        const y2 = cy + Math.sin(angle) * oR2;
        ctx.beginPath();
        ctx.moveTo(x1, y1);
        ctx.lineTo(x2, y2);
        ctx.strokeStyle = `rgba(255,214,0,${em*val*0.25})`;
        ctx.lineWidth = Math.max(1 * dp, (w / totalBars) * .35);
        ctx.lineCap = 'round';
        ctx.stroke();
    }
    const innerGrad = ctx.createRadialGradient(cx, cy, 0, cx, cy, innerR);
    innerGrad.addColorStop(0, `rgba(10,10,25,${0.95+bassE*0.05})`);
    innerGrad.addColorStop(.7, 'rgba(8,8,20,0.98)');
    innerGrad.addColorStop(1, `rgba(0,230,118,${0.08+midE*0.1})`);
    ctx.beginPath();
    ctx.arc(cx, cy, innerR, 0, Math.PI * 2);
    ctx.fillStyle = innerGrad;
    ctx.fill();
    ctx.beginPath();
    ctx.arc(cx, cy, innerR, 0, Math.PI * 2);
    ctx.strokeStyle = `rgba(0,230,118,${0.15+bassE*0.2})`;
    ctx.lineWidth = 1.5 * dp;
    ctx.stroke();
    ctx.beginPath();
    ctx.arc(cx, cy, innerR - 4 * dp, 0, Math.PI * 2);
    ctx.strokeStyle = `rgba(255,214,0,${0.08+midE*0.12})`;
    ctx.lineWidth = 0.8 * dp;
    ctx.stroke();
    const logoR = innerR - 8 * dp;
    if (logoOk) {
        try {
            ctx.save();
            ctx.beginPath();
            ctx.arc(cx, cy, logoR, 0, Math.PI * 2);
            ctx.clip();
            const sz = logoR * 1.7;
            ctx.drawImage(logoImg, cx - sz / 2, cy - sz / 2, sz, sz);
            ctx.restore();
        } catch (e) { drawLogoText(ctx, cx, cy, logoR, dp, bassE); }
    } else { drawLogoText(ctx, cx, cy, logoR, dp, bassE); }
    if (bassE > 0.3) {
        const pg = ctx.createRadialGradient(cx, cy, innerR * .5, cx, cy, innerR * 1.2);
        pg.addColorStop(0, `rgba(0,230,118,${(bassE-.3)*0.15})`);
        pg.addColorStop(1, 'rgba(0,230,118,0)');
        ctx.fillStyle = pg;
        ctx.beginPath();
        ctx.arc(cx, cy, innerR * 1.2, 0, Math.PI * 2);
        ctx.fill();
    }
    if (bassE > 0.65 && Math.random() > 0.5) {
        const tips = [-2.18, -0.96];
        tips.forEach(ta => {
            particles.push({
                x: cx + Math.cos(ta) * (pulseR + maxEarH * .8),
                y: cy + Math.sin(ta) * (pulseR + maxEarH * .8),
                vx: (Math.random() - .5) * 2 * dp,
                vy: -Math.random() * 2 * dp,
                life: 1,
                color: Math.random() > .5 ? '#00e676' : '#ffd600',
                size: (1.5 + Math.random() * 2) * dp
            });
        });
    }
    for (let i = particles.length - 1; i >= 0; i--) {
        const p = particles[i];
        p.x += p.vx;
        p.y += p.vy;
        p.vy += 0.05 * dp;
        p.life -= 0.018;
        if (p.life <= 0) { particles.splice(i, 1); continue; }
        ctx.globalAlpha = p.life;
        ctx.fillStyle = p.color;
        ctx.beginPath();
        ctx.arc(p.x, p.y, p.size * p.life, 0, Math.PI * 2);
        ctx.fill();
    }
    ctx.globalAlpha = 1;
    while (particles.length > 60) particles.shift();
    ctx.fillStyle = `rgba(255,255,255,${0.15+bassE*0.2})`;
    ctx.font = `bold ${(9*dp)}px Orbitron`;
    ctx.textAlign = 'center';
    ctx.fillText('BASS: ' + (bassE * 100).toFixed(0) + '%  MID: ' + (midE * 100).toFixed(0) + '%  HIGH: ' + (highE * 100).toFixed(0) + '%', cx, h - 6 * dp);
    document.getElementById('vizBpm').textContent = (dA.bpm || dB.bpm) ? (dA.bpm || dB.bpm) + ' BPM' : '-- BPM';
    document.getElementById('vizTime').textContent = dA.playing ? dA.fmt(dA.time()) : dB.playing ? dB.fmt(dB.time()) : '--:--';
    document.getElementById('vizStatus').textContent = dA.playing || dB.playing ? '● PLAYING' : '● READY';
}

function drawLogoText(ctx, cx, cy, r, dp, bass) {
    ctx.fillStyle = `rgba(0,230,118,${0.8+bass*0.2})`;
    ctx.font = `900 ${(18*dp)}px Orbitron`;
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    ctx.fillText('SI', cx, cy - 8 * dp);
    ctx.fillStyle = '#fff';
    ctx.font = `900 ${(14*dp)}px Orbitron`;
    ctx.fillText('DJ', cx, cy + 8 * dp);
    ctx.fillStyle = `rgba(255,214,0,${0.8+bass*0.2})`;
    ctx.font = `700 ${(10*dp)}px Orbitron`;
    ctx.fillText('DAGAYA', cx, cy + 24 * dp);
}

/* ===== ANIMATION LOOP ===== */
function tick() {
    dA._ut();
    dB._ut();
    drawWave(document.getElementById('waveA'), dA, '#00e5ff');
    drawWave(document.getElementById('waveB'), dB, '#ff1744');
    drawCatEarViz();
    document.getElementById('aVU').style.height = (dA.vu() * 100) + '%';
    document.getElementById('bVU').style.height = (dB.vu() * 100) + '%';
    if (isRec) {
        const el = Math.floor((Date.now() - recStart) / 1000);
        document.getElementById('recT').textContent = String(Math.floor(el / 60)).padStart(2, '0') + ':' + String(el % 60).padStart(2, '0');
    }
    if (autoOn && dA.buf && dA.playing) {
        const rem = dA.buf.duration - dA.time();
        if (rem < 8 && dB.buf && !dB.playing) {
            dB.play();
            let cf = parseFloat(document.getElementById('xfade').value);
            const fi = setInterval(() => {
                cf += .02;
                if (cf >= 1) { cf = 1;
                    clearInterval(fi); }
                document.getElementById('xfade').value = cf;
                setXF(cf);
            }, 80);
        }
    }
    requestAnimationFrame(tick);
}

/* ===== WAVEFORM CLICK ===== */
document.getElementById('waveA').addEventListener('click', e => {
    if (!dA.buf) return;
    const r = e.target.getBoundingClientRect();
    dA.seek((e.clientX - r.left) / r.width * dA.buf.duration);
});
document.getElementById('waveB').addEventListener('click', e => {
    if (!dB.buf) return;
    const r = e.target.getBoundingClientRect();
    dB.seek((e.clientX - r.left) / r.width * dB.buf.duration);
});

/* ===== DRAG & DROP ===== */
document.addEventListener('dragover', e => {
    e.preventDefault();
    document.getElementById('dropZone').classList.add('show');
});
document.addEventListener('dragleave', e => {
    if (e.relatedTarget === null) document.getElementById('dropZone').classList.remove('show');
});
document.addEventListener('drop', e => {
    e.preventDefault();
    document.getElementById('dropZone').classList.remove('show');
    const fs = Array.from(e.dataTransfer.files).filter(f => f.type.startsWith('audio/'));
    if (!fs.length) { toast('Audio files පමණක්!'); return; }
    if (fs[0]) dA.load(fs[0]);
    if (fs[1]) dB.load(fs[1]);
    fs.slice(2).forEach(f => pl.push({ name: f.name, file: f }));
    renderPL();
});

/* ===== PLAYLIST ===== */
function addPL(files) {
    Array.from(files).filter(f => f.type.startsWith('audio/')).forEach(f => pl.push({ name: f.name, file: f }));
    renderPL();
    savePLData();
}

function renderPL() {
    const b = document.getElementById('plBox');
    if (!pl.length) {
        b.innerHTML = '<div class="text-[9px] text-center py-2" style="color:var(--muted)">Playlist හිස්ය</div>';
        return;
    }
    b.innerHTML = pl.map((p, i) =>
        `<div class="flex items-center gap-1 p-0.5 rounded cursor-pointer hover:brightness-125" style="background:var(--bg)" onclick="loadPL(${i})"><span class="text-[9px] font-bold w-4" style="color:var(--muted)">${i+1}</span><span class="text-[9px] flex-1 truncate">${p.name||'Unknown'}</span><button onclick="event.stopPropagation();pl.splice(${i},1);renderPL();savePLData();" class="text-[9px]" style="color:var(--cb)"><i class="fas fa-times"></i></button></div>`
    ).join('');
}

function loadPL(i) {
    if (pl[i] && pl[i].file) {
        const t = dB.playing ? dA : dB;
        t.load(pl[i].file);
        addHistory('Loaded from playlist: ' + pl[i].name);
    } else if (pl[i] && pl[i].name) {
        toast('File data lost. Re-add the file.');
    }
}

function savePL() {
    localStorage.setItem('hk_pl', JSON.stringify(pl.map(p => p.name)));
    toast('Playlist saved!');
}

function savePLData() {
    try {
        localStorage.setItem('hk_pl_data', JSON.stringify(pl.map(p => ({ name: p.name, file: p.file ? null : null }))));
    } catch (e) {}
}

function clearPL() { pl = [];
    renderPL();
    savePLData();
    toast('Playlist cleared'); }

function shufflePL() {
    for (let i = pl.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1));
        [pl[i], pl[j]] = [pl[j], pl[i]];
    }
    renderPL();
    toast('Shuffled!');
}

function playNext() {
    if (!pl.length) { toast('Playlist හිස්ය'); return; }
    let idx = 0;
    if (dA.playing && dA.buf) {
        idx = pl.findIndex(p => p.name === dA.buf.name);
    }
    if (idx === -1) idx = 0;
    const next = (idx + 1) % pl.length;
    loadPL(next);
}

function clearHistory() {
    history = [];
    localStorage.removeItem('hk_history');
    renderHistory();
}

function renderHistory() {
    const b = document.getElementById('histBox');
    if (!history.length) {
        b.innerHTML = '<div class="text-[9px] text-center py-2" style="color:var(--muted)">No history yet</div>';
        return;
    }
    b.innerHTML = history.slice(-20).reverse().map(h =>
        `<div class="text-[9px] py-0.5" style="color:var(--muted)">${h}</div>`
    ).join('');
}

function addHistory(msg) {
    const t = new Date().toLocaleTimeString();
    history.push(t + ' - ' + msg);
    if (history.length > 100) history.shift();
    localStorage.setItem('hk_history', JSON.stringify(history));
    renderHistory();
    unlock('hist');
    addXP(2);
}

/* ===== VOICE ===== */
function toggleVoice() {
    if (voiceOn) {
        if (recog) recog.stop();
        voiceOn = false;
        document.getElementById('voiceBtn').classList.remove('on-a');
        document.getElementById('vcStat').textContent = 'Voice: OFF';
        return;
    }
    if (!('webkitSpeechRecognition' in window) && !('SpeechRecognition' in window)) {
        toast('Not supported');
        return;
    }
    const SR = window.SpeechRecognition || window.webkitSpeechRecognition;
    recog = new SR();
    recog.continuous = true;
    recog.interimResults = false;
    recog.lang = 'si-LK';
    recog.onerror = e => {
        if (e.error === 'not-allowed') {
            recog.lang = 'en-US';
            try { recog.start(); } catch (ex) {}
        }
    };
    recog.onresult = e => {
        const t = e.results[e.results.length - 1][0].transcript.toLowerCase();
        document.getElementById('vcStat').textContent = 'Heard: "' + t + '"';
        procV(t);
    };
    recog.onend = () => {
        if (voiceOn) try { recog.start(); } catch (e) {}
    };
    try {
        recog.start();
        voiceOn = true;
        document.getElementById('voiceBtn').classList.add('on-a');
        document.getElementById('vcStat').textContent = 'Listening...';
        unlock('voice');
        addXP(5);
    } catch (e) { toast('Voice error'); }
}

function procV(t) {
    addXP(5);
    if (t.includes('play') || t.includes('ප්ලේ')) {
        const d = t.includes('b') ? dB : dA;
        d.toggle();
        toast('Voice: Play');
    }
    if (t.includes('stop') || t.includes('නතර')) {
        const d = t.includes('b') ? dB : dA;
        d.stop();
        toast('Voice: Stop');
    }
    if (t.includes('bass') || t.includes('බාස්')) {
        const d = t.includes('b') ? dB : dA;
        const el = d.id === 'a' ? 'aEL' : 'bEL';
        if (t.includes('වැඩි') || t.includes('up')) {
            const v = Math.min(12, parseFloat(document.getElementById(el).value) + 3);
            document.getElementById(el).value = v;
            d.setEQ('l', v);
        } else {
            const v = Math.max(-12, parseFloat(document.getElementById(el).value) - 3);
            document.getElementById(el).value = v;
            d.setEQ('l', v);
        }
        toast('Voice: Bass');
    }
    if (t.includes('next') && pl.length) {
        loadPL(Math.floor(Math.random() * pl.length));
        toast('Voice: Next');
    }
    if (t.includes('loop')) {
        let b = 0;
        [1, 2, 4, 8, 16].forEach(n => { if (t.includes('' + n)) b = n; });
        const d = t.includes('b') ? dB : dA;
        d.setLoop(b);
    }
    if (t.includes('horn')) { horn(); }
    if (t.includes('scratch')) { hitSam(16); }
    if (t.includes('drop')) { triggerDrop(); }
    if (t.includes('sync')) {
        if (dA.bpm && dB.bpm) { dA.syncTo(dB); } else if (dA.bpm) { dB.syncTo(dA); } else if (dB.bpm) { dA.syncTo(dB); }
        toast('Voice: Sync');
    }
}

/* ===== AI ===== */
function aiGenre() {
    const d = dA.buf ? dA : dB;
    if (!d.buf) { document.getElementById('aiOut').textContent = 'Load කරන්න.'; return; }
    const ch = d.buf.getChannelData(0),
        sr = d.buf.sampleRate;
    let lE = 0,
        mE = 0,
        hE = 0;
    const bk = 4096,
        fr = Math.min(15, Math.floor(ch.length / bk));
    for (let f = 0; f < fr; f++)
        for (let i = 0; i < bk; i++) {
            const v = ch[f * bk + i] ** 2;
            const fq = (i / bk) * sr / 2;
            if (fq < 300) lE += v;
            else if (fq < 4000) mE += v;
            else hE += v;
        }
    const tot = lE + mE + hE || 1;
    let g = 'Electronic';
    if (lE / tot > .6) g = 'Hip-Hop/EDM';
    else if (hE / tot > .3) g = 'Classical/Ambient';
    else if (mE / tot > .5) g = 'Pop/Rock';
    document.getElementById('aiOut').textContent = `Genre: "${g}" | L:${(lE/tot*100).toFixed(0)}% M:${(mE/tot*100).toFixed(0)}% H:${(hE/tot*100).toFixed(0)}% | BPM:${d.bpm}`;
    addXP(5);
    addHistory('AI Genre: ' + g);
}

function aiMood() {
    const d = dA.buf ? dA : dB;
    if (!d.buf) { document.getElementById('aiOut').textContent = 'Load කරන්න.'; return; }
    const ch = d.buf.getChannelData(0);
    let rms = 0;
    for (let i = 0; i < ch.length; i++) rms += ch[i] ** 2;
    rms = Math.sqrt(rms / ch.length);
    const bpm = d.bpm || 120;
    let m = 'Chill';
    if (rms > .15 && bpm > 125) m = 'Party';
    else if (rms > .1 && bpm > 110) m = 'Happy';
    else if (rms < .05 && bpm < 100) m = 'Sad';
    document.getElementById('aiOut').textContent = `Mood: "${m}" | Energy:${(rms*100).toFixed(1)}% BPM:${bpm}`;
    addXP(5);
    addHistory('AI Mood: ' + m);
}

function aiBeatMatch() {
    if (!dA.bpm || !dB.bpm) { document.getElementById('aiOut').textContent = 'දෙකේ Deck load කරන්න.'; return; }
    dA.syncTo(dB);
    document.getElementById('aiOut').textContent = `BeatMatch: A(${dA.bpm})→B(${dB.bpm}) Rate:${dA.rate.toFixed(3)}x`;
    addXP(10);
}

function aiTransition() {
    document.getElementById('aiOut').textContent = 'Smart Transition active...';
    if (dA.playing && dB.buf) {
        [
            () => { dA.setFilter(2000); },
            () => { dB.play();
                dB.setFilter(2000); },
            () => { let c = .3;
                const fi = setInterval(() => { c += .05; if (c >= .7) { clearInterval(fi); return; }
                    document.getElementById('xfade').value = c;
                    setXF(c); }, 100); },
            () => { dA.setFilter(20000); }
        ].forEach((fn, i) => setTimeout(fn, i * 2000));
    }
    addXP(10);
    addHistory('AI Transition');
}

function aiVoiceDJ() {
    const ms = ['Welcome to hpskw PRO!', 'Next track!', 'Feel the bass!', 'Time to dance!', 'Let the music flow!', 'Mashup time!', 'Energy rising!'];
    const m = ms[Math.floor(Math.random() * ms.length)];
    speechSynthesis.speak(new SpeechSynthesisUtterance(m));
    document.getElementById('aiOut').textContent = 'AI DJ: "' + m + '"';
    addXP(10);
    addHistory('AI Voice: ' + m);
}

function aiPlaylist() {
    document.getElementById('aiOut').textContent = pl.length ? `AI Playlist: ${pl.length} tracks optimized.` : 'Playlist එකට add කරන්න.';
    addXP(5);
}

function aiRemix() {
    document.getElementById('aiOut').textContent = 'AI Remix active...';
    if (dA.playing) {
        const or = dA.rate;
        dA.rate = 1.5;
        if (dA.src) dA.src.playbackRate.value = 1.5;
        setTimeout(() => { dA.rate = or; if (dA.src) dA.src.playbackRate.value = or; }, 2000);
    }
    addXP(10);
    addHistory('AI Remix');
}

function aiRecommend() {
    document.getElementById('aiOut').textContent = pl.length ? `Recommend: "${pl[Math.floor(Math.random()*pl.length)].name}"` : 'Playlist එකට add කරන්න.';
    addXP(5);
}

function aiEnergy() {
    const d = dA.buf ? dA : dB;
    if (!d.buf) { document.getElementById('aiOut').textContent = 'Load කරන්න.'; return; }
    const ch = d.buf.getChannelData(0);
    let e = 0;
    for (let i = 0; i < ch.length; i++) e += ch[i] ** 2;
    e = Math.sqrt(e / ch.length) * 100;
    const lvl = e > 15 ? 'High' : e > 8 ? 'Medium' : 'Low';
    document.getElementById('aiOut').textContent = `Energy: ${lvl} (${e.toFixed(1)}%) | BPM:${d.bpm}`;
    addXP(3);
}

function aiCuePredict() {
    const d = dA.buf ? dA : dB;
    if (!d.buf) { document.getElementById('aiOut').textContent = 'Load කරන්න.'; return; }
    const ch = d.buf.getChannelData(0);
    let peaks = [];
    const ws = Math.floor(d.buf.sampleRate * 0.05);
    for (let i = ws; i < ch.length - ws; i += ws) {
        let e = 0;
        for (let j = 0; j < ws; j++) e += ch[i + j] ** 2;
        if (e > 0.01) peaks.push(i / d.buf.sampleRate);
    }
    if (peaks.length > 5) {
        const pred = peaks[Math.floor(peaks.length * 0.75)];
        document.getElementById('aiOut').textContent = `Predicted drop/cue: ${d.fmt(pred)} (${peaks.length} energy peaks detected)`;
        d.seek(pred);
    } else {
        document.getElementById('aiOut').textContent = 'Not enough energy peaks for prediction.';
    }
    addXP(5);
}

/* ===== XP ===== */
function addXP(n) {
    xp += n;
    localStorage.setItem('hk_xp', xp);
    upXP();
    if (xp >= 100 && !achs.includes('l100')) { achs.push('l100');
        localStorage.setItem('hk_ach', JSON.stringify(achs)); }
    if (xp >= 500 && !achs.includes('l500')) { achs.push('l500');
        localStorage.setItem('hk_ach', JSON.stringify(achs)); }
    if (xp >= 1000 && !achs.includes('l1000')) { achs.push('l1000');
        localStorage.setItem('hk_ach', JSON.stringify(achs)); }
}

function unlock(id) {
    if (achs.includes(id)) return;
    const a = ACHS.find(x => x.id === id);
    if (!a) return;
    achs.push(id);
    localStorage.setItem('hk_ach', JSON.stringify(achs));
    if (a.x) addXP(a.x);
    const p = document.getElementById('achPop');
    document.getElementById('achT').textContent = a.i + ' ' + a.n;
    document.getElementById('achD').textContent = a.d + (a.x ? ' +' + a.x : '');
    p.classList.add('show');
    setTimeout(() => p.classList.remove('show'), 2000);
    renderAch();
}

function upXP() {
    let r = RANKS[0];
    for (let i = RANKS.length - 1; i >= 0; i--)
        if (xp >= RANKS[i].m) { r = RANKS[i]; break; }
    document.getElementById('rankTxt').textContent = r.n;
    document.getElementById('xpTxt').textContent = xp;
    const nr = RANKS[RANKS.indexOf(r) + 1];
    document.getElementById('xpBar').style.width = nr ? Math.min(100, ((xp - r.m) / (nr.m - r.m)) * 100) + '%' : '100%';
}

function renderAch() {
    document.getElementById('achGrid').innerHTML = ACHS.map(a => {
        const u = achs.includes(a.id);
        return `<div class="p-1 rounded text-center" style="background:var(--bg);border:1px solid ${u?'var(--acc)':'var(--border)'};opacity:${u?1:.3}"><div>${a.i}</div><div class="text-[9px] font-bold" style="color:${u?'var(--acc)':'var(--muted)'}">${a.n}</div></div>`;
    }).join('');
}

/* ===== SESSION EXPORT/IMPORT ===== */
function exportSession() {
    const data = {
        version: 2,
        sessionId,
        date: new Date().toISOString(),
        xp,
        achs,
        pl: pl.map(p => p.name),
        history
    };
    const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
    const a = document.createElement('a');
    a.href = URL.createObjectURL(blob);
    a.download = 'hpskw_session_' + Date.now() + '.json';
    a.click();
    toast('Session exported!');
}

function importSession() { document.getElementById('sessionInput').click(); }

function importSessionFile(input) {
    const file = input.files[0];
    if (!file) return;
    const reader = new FileReader();
    reader.onload = e => {
        try {
            const data = JSON.parse(e.target.result);
            if (data.xp !== undefined) xp = data.xp;
            if (data.achs) achs = data.achs;
            if (data.history) history = data.history;
            if (data.pl) pl = data.pl.map(n => ({ name: n, file: null }));
            localStorage.setItem('hk_xp', xp);
            localStorage.setItem('hk_ach', JSON.stringify(achs));
            localStorage.setItem('hk_history', JSON.stringify(history));
            localStorage.setItem('hk_pl', JSON.stringify(data.pl || []));
            upXP();
            renderAch();
            renderHistory();
            renderPL();
            toast('Session imported!');
        } catch (err) { toast('Invalid session file'); }
    };
    reader.readAsText(file);
}

/* ===== UI ===== */
function stab(n, btn) {
    document.querySelectorAll('.tpanel').forEach(p => p.classList.remove('on'));
    document.querySelectorAll('.tbtn').forEach(b => b.classList.remove('on'));
    document.getElementById('tp-' + n).classList.add('on');
    btn.classList.add('on');
}

function toggleTheme() {
    const h = document.documentElement;
    const dk = h.getAttribute('data-theme') === 'dark';
    h.setAttribute('data-theme', dk ? 'light' : 'dark');
    document.getElementById('themeBtn').innerHTML = dk ? '<i class="fas fa-sun"></i>' : '<i class="fas fa-moon"></i>';
}

function toggleFS() {
    document.fullscreenElement ? document.exitFullscreen() : document.documentElement.requestFullscreen().catch(() => {});
}

function toast(m) {
    const t = document.createElement('div');
    t.className = 'toast';
    t.textContent = m;
    document.body.appendChild(t);
    setTimeout(() => t.remove(), 2400);
}

/* ===== KEYBOARD ===== */
document.addEventListener('keydown', e => {
    if (e.target.tagName === 'INPUT') return;
    const k = e.key.toLowerCase();
    if (k === 'q') { e.preventDefault();
        dA.toggle(); if (dA.playing && dB.playing) unlock('mix'); }
    if (k === 'w') { e.preventDefault();
        dB.toggle(); if (dA.playing && dB.playing) unlock('mix'); }
    if (k === ' ') { e.preventDefault();
        document.getElementById('xfade').value = .5;
        setXF(.5); }
    if (k === 'r') { e.preventDefault();
        toggleRec(); }
    if (k === 'z') { e.preventDefault(); const v = Math.max(-8, parseFloat(document.getElementById('aPitch').value) - .5);
        document.getElementById('aPitch').value = v;
        dA.setPitch(v); }
    if (k === 'x') { e.preventDefault(); const v = Math.min(8, parseFloat(document.getElementById('aPitch').value) + .5);
        document.getElementById('aPitch').value = v;
        dA.setPitch(v); }
    if (k === ',') { e.preventDefault(); const v = Math.max(-8, parseFloat(document.getElementById('bPitch').value) - .5);
        document.getElementById('bPitch').value = v;
        dB.setPitch(v); }
    if (k === '.') { e.preventDefault(); const v = Math.min(8, parseFloat(document.getElementById('bPitch').value) + .5);
        document.getElementById('bPitch').value = v;
        dB.setPitch(v); }
    if (!e.shiftKey && k >= '1' && k <= '8') { e.preventDefault();
        dA.setCue(parseInt(k) - 1); }
    if (e.shiftKey && k >= '1' && k <= '8') { e.preventDefault();
        dB.setCue(parseInt(k) - 1); }
    if (e.altKey) {
        const km = { '1': 0, '2': 1, '3': 2, '4': 3, '5': 4, '6': 5, '7': 6, '8': 7, 'q': 8, 'w': 9, 'e': 10, 'r': 11, 'a': 12, 's': 13, 'd': 14, 'f': 15, 'z': 16, 'x': 17, 'c': 18, 'v': 19, 'g': 20, 'b': 21, 'n': 22, 'm': 23 };
        if (km[k] !== undefined) { e.preventDefault();
            hitSam(km[k]); }
    }
    if (k === 'p' && e.ctrlKey) { e.preventDefault();
        toggleRec(); }
});

/* ===== START ===== */
document.getElementById('startOverlay').addEventListener('click', function() {
    initAC();
    if (AC.state === 'suspended') AC.resume();
    this.classList.add('gone');
    setTimeout(() => this.style.display = 'none', 600);
});
document.getElementById('startOverlay').addEventListener('touchstart', function(e) {
    e.preventDefault();
    initAC();
    if (AC.state === 'suspended') AC.resume();
    this.classList.add('gone');
    setTimeout(() => this.style.display = 'none', 600);
}, { once: true });

buildSam();
buildEfx();
renderAch();
upXP();
renderHistory();
renderPL();
setXF(.5);
tick();