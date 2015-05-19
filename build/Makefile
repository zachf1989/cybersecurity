S = scala
SC = scalac
SRC = src
CP = classes
SOURCES = $(shell ls $(SRC)/*.scala)
TARGETA = logappend
TARGETB = logread
ARGS = \"$$\@\"

.SUFFIXES: .scala .class

.scala.class:
	@$(SC) -cp $(SRC) -d $(SRC)/$(CP) $(SRC)/*.scala

default: .scala.class
	 @echo DIR=$$\(dirname \"\$${BASH_SOURCE[0]}\"\) > $(TARGETA)
	 @echo $(S) -cp \$$DIR/$(SRC)/$(CP) $(TARGETA) $(ARGS) >> $(TARGETA)

	 @echo DIR=$$\(dirname \"\$${BASH_SOURCE[0]}\"\) > $(TARGETB)
	 @echo $(S) -cp \$$DIR/$(SRC)/$(CP) $(TARGETB) $(ARGS) >> $(TARGETB)
	 @chmod +x $(TARGETA)
	 @chmod +x $(TARGETB)

clean:
	@$(RM) $(TARGETA) $(TARGETB)
	@$(RM) -r -f $(SRC)/$(CP)/*
