/*
	Dump SuperCollider UGens as Common Lisp-compatible s-expressions.

	This file is part of cl-collider.

	UGens are dumped using the sclang binary included with SuperCollider. They are dumped as s-expressions rather than defugen forms in order to, if needed, allow UGen data to be manipulated in Lisp prior to generating the defugen.

	Typically you would run this from Lisp by calling:
	(cl-collider:auto-generate-defugens)

	If you need to call this script from the command line, run something like this:
	sclang -u 57920 dump-ugens.scd dumped-ugens.lisp
	...Where:
	- 57920 is the UDP port for the temporary sclang instance to listen on (to avoid conflicts with existing sclang processes).
	- dump-ugens.scd is the path to this script.
	- dumped-ugens.lisp is the filename to write the UGen s-expressions to.

	One caveat of this approach is that ALL UGens are dumped. Potential problems of importing all UGens with this script:
	- Those which are only defined as sclang code are included. These obviously cannot be run from Lisp.
	- Some UGens (such as Out, DynKlank, etc) require special code to handle.
	The solution is that some UGens should be ignored rather than auto-generated on the Lisp side. Refer to the cl-collider variable *auto-generate-ugen-exclusions* for this.

	Related information:
	- https://github.com/sonoro1234/Lua2SC/blob/master/lua2SC/genugens/scugendumpjson.scd - the script that this one was inspired by.
	- https://github.com/byulparan/cl-collider/issues/46 - "documentation on Emacs"
	- https://github.com/supercollider/supercollider/issues/2300 - "UGen metadata system"
*/

{
	var rate_types, sc_to_lisp, outputfile, out;

	rate_types = [\ir, \kr, \ar];

	sc_to_lisp = {
		| object |
		switch(object.class,
			Nil, {
				"nil"; // i'm not sure, but let's hope this won't cause conflicts with False.
			},
			True, {
				"t";
			},
			False, {
				"nil";
			},
			Symbol, {
				(":" ++ object.asString);
			},
			Array, {
				var res = "(";
				object.do({
					| elem index |
					if(index != 0, {
						res = res ++ " ";
					});
					res = res ++ sc_to_lisp.(elem);
				});
				res = res ++ ")";
				res;
			},
			{
				// Floats, Integers, and Strings are the same in Lisp as they are in SC.
				// anything else, we cross our fingers and hope for the best.
				if([Float, Integer, String].includes(object.class).not, {
					"Printing compileString for unexpected class %: %".format(object.class, object.cs).warn;
				});
				object.cs;
			});
	};

	out = {
		| output |
		outputfile.write(output);
	};

	#[\internal, \local].do({ // use a non-default port to avoid conflicts with already-running servers.
		| s |
		s = Server.perform(s);
		s.addr_(NetAddr("127.0.0.1", 57930)); // use a non-default port to attempt to avoid conflicts with already-running scsynth processes.
	});

	s.waitForBoot({
		if(thisProcess.argv.size < 1, {
			"You must specify an output file as a command line argument to the script.\nFor example: scsynth -u 57920 dump-ugens.scd dumped-ugens.lisp".error;
			s.quit;
			1.exit;
		}, {
			outputfile = File(thisProcess.argv[0], "w");
		});

		UGen.allSubclasses.sort({
			| a b |
			a.name < b.name;
		}).do({
			| ugen_name |
			var parent, supported_methods, method, arg_names, arg_defaults, arg_string, documentation, doc_summary, doc_categories, doc_related, doc_keywords, doc_class_methods, doc_instance_methods, doc_private_class_methods, doc_private_instance_methods, doc_undocumented_class_methods, doc_undocumented_instance_methods;
			parent = ugen_name.superclass;
			supported_methods = rate_types.select({
				| rate_type |
				ugen_name.class.findRespondingMethodFor(rate_type).notNil;
			});
			method = ugen_name.class.findRespondingMethodFor(supported_methods.first);
			if(method.notNil, {
				arg_names = method.argNames.copyRange(1, method.argNames.size);
				arg_defaults = method.prototypeFrame.copyRange(1, method.prototypeFrame.size);
				ugen_name.postcs;
				arg_names.postcs;
				if(arg_names.size == 0, {
					arg_string = "()";
				}, {
					arg_string = sc_to_lisp.([
						arg_names.collect({
							| arg_name |
							arg_name.asString;
						}),
						arg_defaults
					].flop);
				});
			});
			documentation = SCDoc.documents.at("Classes/"++ugen_name.asString);
			if(documentation.notNil, {
				doc_summary = documentation.summary;
				doc_categories = documentation.categories;
				doc_related = documentation.related;
				doc_keywords = documentation.keywords;
				doc_class_methods = documentation.doccmethods;
				doc_instance_methods = documentation.docimethods;
				doc_private_class_methods = documentation.privcmethods;
				doc_private_instance_methods = documentation.privimethods;
				doc_undocumented_class_methods = documentation.undoccmethods;
				doc_undocumented_instance_methods = documentation.undocimethods;
			});
			out.("(:ugen \"%\" :superclass \"%\" :methods % :arguments % :summary % :categories % :related % :keywords %)\n\n".format( // :class-methods % :instance-methods :private-class-methods % :private-instance-methods % :undocumented-class-methods % :undocumented-instance-methods %
				ugen_name,
				parent,
				sc_to_lisp.(supported_methods),
				arg_string,
				sc_to_lisp.(doc_summary),
				sc_to_lisp.(doc_categories),
				sc_to_lisp.(doc_related),
				sc_to_lisp.(doc_keywords),
				// sc_to_lisp.(doc_class_methods),
				// sc_to_lisp.(doc_instance_methods),
				// sc_to_lisp.(doc_private_class_methods),
				// sc_to_lisp.(doc_private_instance_methods),
				// sc_to_lisp.(doc_undocumented_class_methods),
				// sc_to_lisp.(doc_undocumented_instance_methods),
			));
		});

		outputfile.close;
		s.quit;
		0.exit;

	});

}.value;
