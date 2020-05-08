TSLCHECK=tslcheck
TSLSIZE=tslsize
TSLSYM=tslsym
TSL2TLSF=tsl2tlsf
TSLSPLIT=tslsplit
TSLPLAY=tslplay
CFMCHECK=cfmcheck
CFMINFO=cfminfo
CFMSYM=cfmsym
CFM2CODE=cfm2code

TOOLS=\
  ${TSLCHECK}\
  ${TSLSIZE}\
  ${TSLSYM}\
  ${TSL2TLSF}\
  ${TSLSPLIT}\
  ${TSLPLAY}\
  ${CFMCHECK}\
  ${CFMINFO}\
  ${CFMSYM}\
  ${CFM2CODE}

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

${TSLSPLIT}:
	${BLDTOOL} build :$@
	@if [ -d "dist" ]; then cp ./dist/build/$@/$@ $@; else cp ${STACKPATH}/bin/$@ $@; fi

${TSL2TLSF}:
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

install:
	${BLDTOOL} install

test:
	${BLDTOOL} test

doc:
	${BLDTOOL} haddock --open

clean:
	${BLDTOOL} clean
	@for i in ${TOOLS}; do rm -f $${i}; done

.PHONY: clean test ${TOOLS}
.SILENT:
