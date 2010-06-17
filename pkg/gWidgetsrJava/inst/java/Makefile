# ... This is the coolest way to rebuild your java
# files selectively after making some changes. Just type 'make'.


# linux box
export CLASSPATH=/home/verzani/R/lib/R/library/rJava/jri/JRI.jar:/home/verzani/R/lib/R/library/JavaGD/java/javaGD.jar:./
# mac
#export CLASSPATH=/Library/Frameworks/R.framework/Resources/library/rJava/libs/i386/:/Library/Frameworks/R.framework/Resources/library/JavaGD/java/javaGD.jar:/.
JAVAC = javac -Xlint
CLASS_FILES = gWidgetsrJava/ActionDispatcher.class \
	gWidgetsrJava/TableSorter.class gWidgetsrJava/gDialog.class \
	gWidgetsrJava/gExpandGroup.class gWidgetsrJava/gRadio.class \
	gWidgetsrJava/TableSorter.class gWidgetsrJava/gTable.class \
	gWidgetsrJava/ColoredTable.class gWidgetsrJava/gDf.class \
	gWidgetsrJava/gGraphics.class \
	gWidgetsrJava/gNotebook.class  gWidgetsrJava/gIdle.class \
	gWidgetsrJava/JXDatePicker.class gWidgetsrJava/JXMonthView.class \
	gWidgetsrJava/JXDatePickerFormatterFactory.class \
	gWidgetsrJava/DateUtils.class gWidgetsrJava/DateSpan.class \


JAR_CMD = jar cvf 
JAR_NAME = gWidgetsrJava.jar

Default: $(CLASS_FILES)
	$(JAR_CMD) $(JAR_NAME) gWidgetsrJava/*class

%.class: %.java
	$(JAVAC) $<

