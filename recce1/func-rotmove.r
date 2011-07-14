rotate<-function (x,y,rad){     #basic rotate point x y to new x y
                                        #around radian angle
    x.prime<-vector()
    y.prime<-vector
{
    x.prime<-x*cos(rad) - y*sin(rad)
    y.prime<-x*sin(rad) + y*cos(rad)
}
return (cbind(x.prime,y.prime))
}

rotmove<-function(alldf){   #creates centered and horizonatal rotated
                                        #x.prime and y.prime points specific to
                                        #each line segment in patterndf
cpx<-alldf[[5]]-alldf[[14]]  #center x points along each segment
cpy<-alldf[[6]]-alldf[[15]]  #center y points along each segment
rad<-alldf[[16]]             #degrees to rotate to horizontal for each segment
pointsR<-rotate(cpx,cpy,rad)
insidedf<-cbind(alldf,pointsR)
return(insidedf)
}


