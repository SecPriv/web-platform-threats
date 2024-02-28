with import (fetchTarball { url = "https://github.com/nixos/nixpkgs/archive/54644f409ab471e87014bb305eac8c50190bcf48.tar.gz"; }) {};

let z3_latest = z3.overrideAttrs (s: p: {
      version = "4.12.3-master";
      src = fetchTarball {
        url = "https://github.com/Z3Prover/z3/archive/1b263f85e4e8f693b455ff3419fe9e914ff78fe6.tar.gz";
        sha256 = "0pxavgcznwkmzm6wp5bcp5fj7hx61kflgzfl88f0jngb8d468pz8";
      };
    });
in

let scripts = stdenv.mkDerivation {
      name = "parse_trace";
      nativeBuildInputs = [ makeWrapper emacs-nox ];
      buildInputs = [ swiProlog  z3_latest ];
      src = ./.;
      dontPatchShebangs = true;
      installPhase = ''
          mkdir -p $out/usr/bin $out/share
          ln -s ${coreutils-full}/bin/env $out/usr/bin/env
          install -D -t $out/bin/ -m 555 *.pl
          # install -D -t $out/share/ -m 444 *.smt
          cat trace_matching.smt | emacs --script ./trace_matching_macroexpand.el > $out/share/trace_matching.smt

          makeWrapper $out/bin/parse_trace.pl $out/bin/parse_trace --prefix PATH : ${lib.makeBinPath [ swiProlog ]} --add-flags $out/share/trace_matching.smt
          makeWrapper $out/bin/smtlib_compat.pl $out/bin/smtlib_compat --prefix PATH : ${lib.makeBinPath [ swiProlog ]}
          makeWrapper $out/bin/z3_wrapper.pl $out/bin/z3_wrapper --prefix PATH : ${lib.makeBinPath [ swiProlog  z3_latest ]}
      '';
      meta = {
        description = "Convert json trace to smtlib query";
      };
    };
in
let wrapper = pkgs.writeScript "parse_trace" ''
      #!${stdenv.shell}
      set -eou pipefail

      if [ $# -lt 2 ]; then
         ${scripts}/bin/parse_trace "$@" | ${scripts}/bin/smtlib_compat
      else
          ${scripts}/bin/parse_trace $1 | ${scripts}/bin/smtlib_compat > $2
      fi
      '';
in
let docker = dockerTools.buildImage {
      name = "wpt-trace-matching";
      tag = "latest";
      copyToRoot = stdenv.mkDerivation {
        name = "wrapper";
        unpackPhase = "true";
        installPhase = ''
          shopt -s extglob

          mkdir -p $out/bin $out/usr/bin $out/tmp
          ln -s ${busybox}/bin/!(date|awk|timeout) $out/bin/
          cp ${coreutils}/bin/env $out/usr/bin/env
          cp ${coreutils}/bin/timeout $out/bin/timeout
          cp ${coreutils}/bin/date $out/bin/date
          cp ${gawk}/bin/awk $out/bin/awk
          cp ${wrapper} $out/bin/parse_trace
          cp ${scripts}/bin/smtlib_compat $out/bin/
          cp ${scripts}/bin/z3_wrapper $out/bin/
          ln -s ${z3_latest}/bin/z3 $out/bin/z3
          ln -s ${redis}/bin/redis-cli $out/bin/redis-cli
          ln -s ${bash}/bin/bash $out/bin/bash
        '';
      };
      config = {
        Cmd = ["/bin/parse_trace"];
        WorkingDir = "/mnt";
      };
    };
in
{ inherit scripts docker; }
