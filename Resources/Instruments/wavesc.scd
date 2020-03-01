//
// Select All and press the Enter key (not the Return key...)
//

Routine {

s.bootSync;

//(
p = {arg dur=1, freq=440, amp=0.2, pan=0;
	var env, z, sig, envctl, ampenv, gate=1, outbus=0;

	env = Env.newClear(32);
	envctl = Control.names([\env]).kr( env.asArray );
	ampenv = EnvGen.kr(envctl, gate, amp, 0, timeScale:dur, doneAction:2);

	sig = SinOsc.ar(freq );
	z = ampenv * sig;
	Out.ar(outbus, Pan2.ar(z, pan, 1));
};
p = SynthDef.new("wavesc", p);
p.send(s);
//)

//(
p = {arg dur=1, freq=440, amp=0.2, pan=0;
	var env, z, sig, envctl, ampenv, skew, skewctl, skewenv, gate=1, outbus=0;

	env = Env.newClear(32);
	envctl = Control.names([\env]).kr( env.asArray );
	ampenv = EnvGen.kr(envctl, gate, amp, 0, timeScale:dur, doneAction:2);

	skew = Env.newClear(32);
	skewctl = Control.names([\skew]).kr( skew.asArray );
	skewenv = EnvGen.kr(skewctl, gate, 1, 0, timeScale:dur);

	sig = SinOsc.ar(freq * skewenv);
	z = ampenv * sig;
	Out.ar(outbus, Pan2.ar(z, pan, 1));
};
p = SynthDef.new("wavesk", p);
p.send(s);
//)

}.play;

/*=======================================================================*
                                 TESTING
 *=======================================================================*

(
~oneone = [1, 1, -99, -99, 1, 1, 5, 0]	;
~onezero = [1, 1, -99, -99, 0, 1, 5, 0] ;
~halfup = [1, 3, -99, -99, 1, 0.45, 5, 0, 1.0591, 0.1, 5, 0, 1.0591, 0.45, 5, 0];
~halfdown = [1, 3, -99, -99, 1, 0.45, 5, 0, 0.944, 0.1, 5, 0, 0.944, 0.45, 5, 0];
~octup = [1, 3, -99, -99, 1, 0.45, 5, 0, 2, 0.1, 5, 0, 2, 0.45, 5, 0];
~octdown = [1, 3, -99, -99, 1, 0.45, 5, 0, 0.5, 0.1, 5, 0, 0.5, 0.45, 5, 0];
~fifthup = [1, 3, -99, -99, 1, 0.45, 5, 0, 1.5, 0.1, 5, 0, 1.5, 0.45, 5, 0];
~fifthdown = [1, 3, -99, -99, 1, 0.45, 5, 0, 0.666, 0.1, 5, 0, 0.666, 0.45, 5, 0];
)

Synth.new("wavesc", [\amp, 0.1, \dur, 2, \freq, 500, \env, ~onezero]);

Synth.new("wavesc", [\amp, 0.1, \dur, 2, \freq, 500, \pan, -1, \env, ~onezero]);

Synth.new("wavesc", [\amp, 0.1, \dur, 2, \freq, 500, \pan, 1, \env, ~onezero]);

Synth.new("wavesk", [\amp, 0.1, \dur, 2, \freq, 500, \env, ~onezero, \skew, ~fifthdown ]);

(
s.sendBundle(0.0, [ "/s_new", "wavesc", -1, 0, 1] ,
	   		    [ "/n_setn", -1, "env", 8, 1, 1, -99, -99, 1, 1, 5, 0  ]
	   		);
)

(
s.sendBundle(0.0, [ "/s_new", "wavesc", -1, 0, 1, "freq", 440] ,
	   		    [ "/n_setn", -1, "env", 8, 0, 3, -99, -99, 1, 0.05, 5, 0, 1, 0.9, 5, 0, 0, 0.05, 5, 0 ]
	   		);
)

(
s.sendBundle(0.0, [ "/s_new", "wavesk", -1, 0, 1, "dur", 2, "freq", 1000  ] ,
	   			[ "/n_setn", -1, "env", 8, 1, 1, -99, -99, 1, 1, 5, 0  ] ,
	   			[ "/n_setn", -1, "skew", 16, 1, 3, -99, -99, 1, 0.45, 5, 0, 0.944, 0.1, 5, 0, 0.944, 0.45, 5, 0]
	   			);
)

(
s.sendBundle(0.0, [ "/s_new", "wavesk", -1, 0, 1, "dur", 2, "freq", 1000  ] ,
	   			[ "/n_setn", -1, "env", 8, 1, 1, -99, -99, 1, 1, 5, 0  ] ,
	   			[ "/n_setn", -1, "skew", 16, 1, 3, -99, -99, 1, 0.45, 5, 0, 0.944, 0.1, 5, 0, 0.944, 0.45, 5, 0]
	   			);
)

