S = scala
SC = scalac
SRC = src
TARGETA = logappend
TARGETB = logread
ARGS = \"$$\@\"

.scala.class:
	@$(SC) -cp $(SRC) -d $(SRC) $(SRC)/*.scala

default: .scala.class
	 @echo \#!/bin/bash > $(TARGETA)
	 @echo DIR=\`dirname \"\$$0\"\` >> $(TARGETA)
	 @echo $(S) -cp \$$DIR/$(SRC) $(TARGETA) $(ARGS) >> $(TARGETA)
	 @echo \#!/bin/bash > $(TARGETB)
	 @echo DIR=\`dirname \"\$$0\"\` >> $(TARGETB)
	 @echo $(S) -cp \$$DIR/$(SRC) $(TARGETB) $(ARGS) >> $(TARGETB)
	 @chmod +x $(TARGETA)
	 @chmod +x $(TARGETB)

clean:
	@$(RM) $(TARGETA) $(TARGETB)
	@$(RM) $(SRC)/*.class
	@$(RM) -r -f $(SRC)/capstone
