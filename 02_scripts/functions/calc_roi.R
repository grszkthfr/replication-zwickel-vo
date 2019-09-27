distance <- function(a_x, a_y, b_x, b_y){
    
    distance <- sqrt((b_x - a_x)**2 + (b_y - a_y)**2)
    return(distance)
}

### http://stephenrho.github.io/visual-angle.html
visAngle <- function(size, distance){
    # this function calculates visual angle
    # size and distance must be in the same units
    Rad = 2*atan(size/(2*distance))
    Ang = Rad*(180/pi)
    return(Ang)
}
