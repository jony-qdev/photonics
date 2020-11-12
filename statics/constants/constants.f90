module constants 

    ! Module of constants physics and mathematics
    !   - PI : real, the relationship between the width of a circle, called the diameter, and what its outer edge measures, called the circumference
    !   - C : real (m/s) : speed of light in vacuum
    !   - EPS_0 : real, relative permittivity of vacuum
    !   - MU_0 : real, relative permeability of vacuum 
    !   - ETA : real, vacuum impedance
 
    implicit none 

    real, parameter ::  PI = acos(-1.0), &
                        C = 3.0e8, &
                        EPS_0 = (1.0 / (36.0 * PI)) * 1e-9, &
                        MU_0 = 4.0 * PI * 1e-7, &
                        ETA = sqrt(MU_0 / EPS_0) 

end module constants 