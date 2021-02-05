TSLCHECK = tslcheck
TSLSIZE = tslsize
TSLSYM   = tslsym
TSL2TLSF = tsl2tlsf

CFMCHECK = cfmcheck
CFMINFO  = cfminfo
CFMSYM   = cfmsym
CFM2CODE = cfm2code

BLDTOOL=`if [ -d "dist" ]; then echo "cabal"; else echo "stack"; fi`

default:
	${BLDTOOL} build
	@if [ -d "dist" ]; then cp ./dist/build/${TSL2TLSF}/${TSL2TLSF} ${TSL2TLSF}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${TSL2TLSF} ${TSL2TLSF}; fi
	@if [ -d "dist" ]; then cp ./dist/build/${TSLCHECK}/${TSLCHECK} ${TSLCHECK}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${TSLCHECK} ${TSLCHECK}; fi
	@if [ -d "dist" ]; then cp ./dist/build/${TSLSIZE}/${TSLSIZE} ${TSLSIZE}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${TSLSIZE} ${TSLSIZE}; fi
	@if [ -d "dist" ]; then cp ./dist/build/${TSLSYM}/${TSLSYM} ${TSLSYM}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${TSLSYM} ${TSLSYM}; fi
	@if [ -d "dist" ]; then cp ./dist/build/${CFMSYM}/${CFMSYM} ${CFMSYM}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${CFMSYM} ${CFMSYM}; fi
	@if [ -d "dist" ]; then cp ./dist/build/${CFMCHECK}/${CFMCHECK} ${CFMCHECK}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${CFMCHECK} ${CFMCHECK}; fi
	@if [ -d "dist" ]; then cp ./dist/build/${CFMINFO}/${CFMINFO} ${CFMINFO}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${CFMINFO} ${CFMINFO}; fi
	@if [ -d "dist" ]; then cp ./dist/build/${CFM2CODE}/${CFM2CODE} ${CFM2CODE}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${CFM2CODE} ${CFM2CODE}; fi


${TSLCHECK}:
	${BLDTOOL} build ${TSLCHECK}
	@if [ -d "dist" ]; then cp ./dist/build/${TSLCHECK}/${TSLCHECK} ${TSLCHECK}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${TSLCHECK} ${TSLCHECK}; fi

${TSLSIZE}:
	${BLDTOOL} build ${TSLSIZE}
	@if [ -d "dist" ]; then cp ./dist/build/${TSLSIZE}/${TSLSIZE} ${TSLSIZE}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${TSLSIZE} ${TSLSIZE}; fi

${TSLSYM}:
	${BLDTOOL} build ${TSLSYM}
	@if [ -d "dist" ]; then cp ./dist/build/${TSLSYM}/${TSLSYM} ${TSLSYM}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${TSLSYM} ${TSLSYM}; fi

${TSL2TLSF}:
	${BLDTOOL} build ${TSL2TLSF}
	@if [ -d "dist" ]; then cp ./dist/build/${TSL2TLSF}/${TSL2TLSF} ${TSL2TLSF}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${TSL2TLSF} ${TSL2TLSF}; fi


${CFMCHECK}:
	${BLDTOOL} build ${CFMCHECK}
	@if [ -d "dist" ]; then cp ./dist/build/${CFMCHECK}/${CFMCHECK} ${CFMCHECK}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${CFMCHECK} ${CFMCHECK}; fi

${CFMINFO}:
	${BLDTOOL} build ${CFMINFO}
	@if [ -d "dist" ]; then cp ./dist/build/${CFMINFO}/${CFMINFO} ${CFMINFO}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${CFMINFO} ${CFMINFO}; fi

${CFMSYM}:
	${BLDTOOL} build ${CFMSYM}
	@if [ -d "dist" ]; then cp ./dist/build/${CFMSYM}/${CFMSYM} ${CFMSYM}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${CFMSYM} ${CFMSYM}; fi

${CFM2CODE}:
	${BLDTOOL} build ${CFM2CODE}
	@if [ -d "dist" ]; then cp ./dist/build/${CFM2CODE}/${CFM2CODE} ${CFM2CODE}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${CFM2CODE} ${CFM2CODE}; fi


ghci:
	${BLDTOOL} repl

install:
	${BLDTOOL} install

haddock:
	${BLDTOOL} haddock

clean:
	${BLDTOOL} clean
	rm -f ${TSL2TLSF}
	rm -f ${TSLCHECK}
	rm -f ${TSLSIZE}
	rm -f ${TSLSYM}
	rm -f ${TSLSYNTH}
	rm -f ${CFMCHECK}
	rm -f ${CFMINFO}
	rm -f ${CFMSYM}
	rm -f ${CFM2CODE}

.PHONY: clean
.SILENT:
