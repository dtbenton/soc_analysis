#####################
# TABLE OF CONTENTS #
#####################

# COND A:
  # RED CUBE DIAMOND CAUSE; GREEN CYLINDER CIRCLE NONCAUSE

# COND B:
  # RED CUBE DIAMOND NONCAUSE; GREEN CYLINDER CIRCLE CAUSE

# COND C:
# RED CUBE CIRCLE CAUSE; GREEN CYLINDER DIAMOND NONCAUSE

# COND D:
# RED CUBE CIRCLE NONCAUSE; GREEN CYLINDER DIAMOND CAUSE

#############
# SOC MODEL #
#############
require(hier.part)


#############              
## OBJECTS ##
#############
objects = data.frame(x = c('1 1 1 1 0 0 0 0 0 0 0 0',
                           '0 0 0 0 1 1 1 1 0 0 0 0',
                           '0 0 0 0 0 0 0 0 1 1 1 1'))
names(objects) = NULL
rownames(objects) = c(1:3)

# Obj1: Cube
# Obj2: Cylinder
# Obj4: Test Object


##############              
## FEATURES ##
##############
feature = data.frame(x = c('1 0', '0 1'))
names(feature) = NULL
rownames(feature) = NULL

# Diamond: 1 0
# Circle: 0 1


##############              
## ACTIVITY ##
##############
activity = data.frame(x = c('0', '1'))
names(activity) = NULL
rownames(activity) = NULL

# Detector "off": 0
# Detector "on": 1


###########
## COLOR ##
###########
color = data.frame(x = c('1 0 0',
                         '0 1 0',
                         '0 0 1'))

names(color) = NULL
rownames(color) = NULL

# Red: 1 0 0
# Green: 0 1 0
# Blue: 0 0 1

##########
# COND A #
##########

# STATIC HABITUATION
sink('SOC_cond_A_static.ex')
cat(paste("defT:-", "\n", sep="\t"))
cat(paste("name: RedCubeDiamond", "\n", sep="\t"))
# INPUT
cat(paste("I:", "\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Feature)", sep="\t"))
print(feature[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(FeatureOUT)", sep="\t"))
print(feature[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


# INPUT
cat(paste("name: GreenCylinderCircle", "\n", sep="\t"))
cat(paste("I:", "\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Feature)", sep="\t"))
print(feature[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(FeatureOUT)", sep="\t"))
print(feature[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()



# DYNAMIC HABITUATION
sink('SOC_cond_A_dynamic.ex')
cat(paste("defT:-", "\n", sep="\t"))
cat(paste("name: RedCubeDiamond", "\n", sep="\t"))
# RED SHAPES #
# INPUT
cat(paste("I:", "\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[1,1], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ActivityOUT)", sep="\t"))
print(activity[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


cat(paste("name: GreenCylinderCircle", "\n", sep="\t"))
cat(paste("I:", "\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[2,1], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ActivityOUT)", sep="\t"))
print(activity[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()





########
# TEST #
########
sink('SOC_cond_A_test.ex')
# CONSISTENT
cat(paste("name: Consistent", "\n", sep="\t"))
# INPUT
cat(paste("I:","\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Feature)", sep="\t"))
print(feature[1,1], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(FeatureOUT)", sep="\t"))
print(feature[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ActivityOUT)", sep="\t"))
print(activity[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


# INCONSISTENT
cat(paste("name: Inconsistent", "\n", sep="\t"))
# INPUT
cat(paste("I:","\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Feature)", sep="\t"))
print(feature[2,1], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(FeatureOUT)", sep="\t"))
print(feature[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ActivityOUT)", sep="\t"))
print(activity[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()

##########
# COND B #
##########

# STATIC HABITUATION
sink('SOC_cond_B_static.ex')
cat(paste("defT:-", "\n", sep="\t"))
cat(paste("name: RedCubeDiamond", "\n", sep="\t"))
# INPUT
cat(paste("I:", "\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Feature)", sep="\t"))
print(feature[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(FeatureOUT)", sep="\t"))
print(feature[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


# INPUT
cat(paste("name: GreenCylinderCircle", "\n", sep="\t"))
cat(paste("I:", "\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Feature)", sep="\t"))
print(feature[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(FeatureOUT)", sep="\t"))
print(feature[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()



# DYNAMIC HABITUATION
sink('SOC_cond_B_dynamic.ex')
cat(paste("defT:-", "\n", sep="\t"))
cat(paste("name: RedCubeDiamond", "\n", sep="\t"))
# RED SHAPES #
# INPUT
cat(paste("I:", "\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[1,1], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ActivityOUT)", sep="\t"))
print(activity[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


cat(paste("name: GreenCylinderCircle", "\n", sep="\t"))
cat(paste("I:", "\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[2,1], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ActivityOUT)", sep="\t"))
print(activity[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()





########
# TEST #
########
sink('SOC_cond_B_test.ex')
# CONSISTENT
cat(paste("name: Consistent", "\n", sep="\t"))
# INPUT
cat(paste("I:","\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Feature)", sep="\t"))
print(feature[2,1], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(FeatureOUT)", sep="\t"))
print(feature[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ActivityOUT)", sep="\t"))
print(activity[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


# INCONSISTENT
cat(paste("name: Inconsistent", "\n", sep="\t"))
# INPUT
cat(paste("I:","\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Feature)", sep="\t"))
print(feature[1,1], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(FeatureOUT)", sep="\t"))
print(feature[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ActivityOUT)", sep="\t"))
print(activity[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()


##########
# COND C #
##########

# STATIC HABITUATION
sink('SOC_cond_C_static.ex')
cat(paste("defT:-", "\n", sep="\t"))
cat(paste("name: RedCubeCircle", "\n", sep="\t"))
# INPUT
cat(paste("I:", "\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Feature)", sep="\t"))
print(feature[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(FeatureOUT)", sep="\t"))
print(feature[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


# INPUT
cat(paste("name: GreenCylinderDiamond", "\n", sep="\t"))
cat(paste("I:", "\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Feature)", sep="\t"))
print(feature[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(FeatureOUT)", sep="\t"))
print(feature[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()



# DYNAMIC HABITUATION
sink('SOC_cond_C_dynamic.ex')
cat(paste("defT:-", "\n", sep="\t"))
cat(paste("name: RedCubeCircle", "\n", sep="\t"))
# RED SHAPES #
# INPUT
cat(paste("I:", "\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[1,1], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ActivityOUT)", sep="\t"))
print(activity[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


cat(paste("name: GreenCylinderDiamond", "\n", sep="\t"))
cat(paste("I:", "\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[2,1], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ActivityOUT)", sep="\t"))
print(activity[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()





########
# TEST #
########
sink('SOC_cond_C_test.ex')
# CONSISTENT
cat(paste("name: Consistent", "\n", sep="\t"))
# INPUT
cat(paste("I:","\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Feature)", sep="\t"))
print(feature[2,1], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(FeatureOUT)", sep="\t"))
print(feature[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ActivityOUT)", sep="\t"))
print(activity[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


# INCONSISTENT
cat(paste("name: Inconsistent", "\n", sep="\t"))
# INPUT
cat(paste("I:","\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Feature)", sep="\t"))
print(feature[1,1], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(FeatureOUT)", sep="\t"))
print(feature[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ActivityOUT)", sep="\t"))
print(activity[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()



##########
# COND D #
##########

# STATIC HABITUATION
sink('SOC_cond_D_static.ex')
cat(paste("defT:-", "\n", sep="\t"))
cat(paste("name: RedCubeCircle", "\n", sep="\t"))
# INPUT
cat(paste("I:", "\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Feature)", sep="\t"))
print(feature[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(FeatureOUT)", sep="\t"))
print(feature[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


# INPUT
cat(paste("name: GreenCylinderDiamond", "\n", sep="\t"))
cat(paste("I:", "\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Feature)", sep="\t"))
print(feature[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(FeatureOUT)", sep="\t"))
print(feature[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()



# DYNAMIC HABITUATION
sink('SOC_cond_D_dynamic.ex')
cat(paste("defT:-", "\n", sep="\t"))
cat(paste("name: RedCubeCircle", "\n", sep="\t"))
# RED SHAPES #
# INPUT
cat(paste("I:", "\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[1,1], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ActivityOUT)", sep="\t"))
print(activity[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


cat(paste("name: GreenCylinderDiamond", "\n", sep="\t"))
cat(paste("I:", "\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[2,1], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ActivityOUT)", sep="\t"))
print(activity[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()





########
# TEST #
########
sink('SOC_cond_D_test.ex')
# CONSISTENT
cat(paste("name: Consistent", "\n", sep="\t"))
# INPUT
cat(paste("I:","\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Feature)", sep="\t"))
print(feature[1,1], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(FeatureOUT)", sep="\t"))
print(feature[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ActivityOUT)", sep="\t"))
print(activity[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


# INCONSISTENT
cat(paste("name: Inconsistent", "\n", sep="\t"))
# INPUT
cat(paste("I:","\n", sep="\t"))

cat(paste("(Shape)", sep="\t"))
print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Color)", sep="\t"))
print(color[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(Feature)", sep="\t"))
print(feature[2,1], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(ShapeOUT)", sep="\t"))
print(objects[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ColorOUT)", sep="\t"))
print(color[3,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(FeatureOUT)", sep="\t"))
print(feature[2,1], sep = "\t", quote = FALSE, row.names = FALSE)

cat(paste("(ActivityOUT)", sep="\t"))
print(activity[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()