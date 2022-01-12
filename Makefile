TSLCHECK=tslcheck
TSLSIZE=tslsize
TSLSYM=tslsym
TSLRESOLVE=tslresolve
TSL2TLSF=tsl2tlsf
TSL2TOML=tsl2toml
TSLSPLIT=tslsplit
TSLPLAY=tslplay
CFMCHECK=cfmcheck
CFMINFO=cfminfo
CFMSYM=cfmsym
CFM2CODE=cfm2code
TSLCOREGEN=tslcoregen
TSLMINREAL=tslminrealizable
PARSEHOA=parsehoa
HOA2CODE=hoa2code

TOOLS=\
  ${TSLCHECK}\
  ${TSLSIZE}\
  ${TSLSYM}\
  ${TSLRESOLVE}\
  ${TSL2TLSF}\
  ${TSL2TOML}\
  ${TSLSPLIT}\
  ${TSLPLAY}\
  ${CFMCHECK}\
  ${CFMINFO}\
  ${CFMSYM}\
  ${CFM2CODE}\
  ${TSLCOREGEN}\
  ${TSLMINREAL}\
  ${PARSEHOA}\
  ${HOA2CODE}

STACKPATH=$(shell if [ -d "dist" ]; then echo ""; else stack path | grep local-install-root | sed 's/local-install-root: //'; fi)
BLDTOOL=$(shell if [ -d "dist" ]; then echo "cabal"; else echo "stack"; fi)

default:
	${BLDTOOL} build
	@for i in ${TOOLS}; do if [ -d "dist" ]; then cp ./dist/build/$${i}/$${i} $${i}; else cp ${STACKPATH}/bin/$${i} $${i}; fi; done

${TSLCHECK}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi

${TSLSIZE}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi

${TSLSYM}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi

${TSLRESOLVE}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi

${TSLSPLIT}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi

${TSL2TLSF}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi

${TSL2TOML}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi

${CFMCHECK}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi

${CFMINFO}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi

${CFMSYM}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi

${CFM2CODE}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi

${TSLPLAY}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi

${TSLCOREGEN}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi

${TSLMINREAL}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi

${PARSEHOA}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi

${HOA2CODE}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi




install:
	${BLDTOOL} install

test:
	${BLDTOOL} test

doc:
	${BLDTOOL} haddock --open

format:
	stylish-haskell  -c .stylish-haskell.yaml  -i  -r src/

clean:
	${BLDTOOL} clean
	@for i in ${TOOLS}; do rm -f $${i}; done

.PHONY: install test doc format clean ${TOOLS}
.SILENT:
