!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_parflow

CONTAINS

!------------------------------------------------------------------

      SUBROUTINE parflow_init(ims, ime, jms, jme, num_soil_layers,  &
     &    PF_FLUX, PF_PRESSURE, PF_POROSITY, PF_SATURATION, filename) 
      INTEGER, INTENT(IN) ::   ims, ime, jms, jme, num_soil_layers
      REAL :: PF_FLUX( ims:ime,  1:num_soil_layers, jms:jme)
      REAL :: PF_PRESSURE(ims:ime,  1:num_soil_layers, jms:jme) 
      REAL :: PF_POROSITY( ims:ime,  1:num_soil_layers, jms:jme)
      REAL :: PF_SATURATION( ims:ime,  1:num_soil_layers, jms:jme)
      character *(*), intent (in)         ::                           &
       filename

! set all vars to zero
    PF_FLUX = 0.
    PF_PRESSURE=0.
    PF_POROSITY = 0.
    PF_SATURATION = 0.
      WRITE(*,*) 'in parflow_init'
#if ( WRF_PFWRF == 1 )
      call wrfparflowinit(filename)
#else
      call wrf_error_fatal ( 'WRF is not compiled to run with ParFlow.' )
#endif
     RETURN

      END SUBROUTINE parflow_init

!..................................................................

      SUBROUTINE parflow_run( BLDT, CURR_SECS, ims, ime, jms, jme,   &
     &               its, ite, jts, jte, num_soil_layers, &
     &               PF_FLUX, PF_PRESSURE, PF_POROSITY, PF_SATURATION) 

      INTEGER, INTENT(IN) ::  ims, ime, jms, jme,its, ite, jts, jte, num_soil_layers
      REAL, INTENT(IN) :: BLDT, CURR_SECS
      INTEGER i,j,k

      INTEGER GHOST_SIZE_I_LOWER, GHOST_SIZE_J_LOWER
      INTEGER GHOST_SIZE_I_UPPER, GHOST_SIZE_J_UPPER

      REAL :: PF_FLUX( ims:ime,  1:num_soil_layers, jms:jme) 
      REAL :: PF_PRESSURE(ims:ime,  1:num_soil_layers, jms:jme)  
      REAL :: PF_POROSITY( ims:ime,  1:num_soil_layers, jms:jme) 
      REAL :: PF_SATURATION( ims:ime,  1:num_soil_layers, jms:jme) 
      REAL*8 PFDT, PF_time


      GHOST_SIZE_I_LOWER=its - ims;
      GHOST_SIZE_J_LOWER=jts - jms;

      GHOST_SIZE_I_UPPER=ime - ite;
      GHOST_SIZE_J_UPPER=jme - jte;

! RMM convert from seconds to hours before sending time to PF
      PFDT = DBLE(BLDT) / 3600.0d0 
      PF_TIME = DBLE(CURR_SECS) / 3600.d0

      WRITE(*,*) 'in parflow_advance t=', CURR_SECS
#if ( WRF_PFWRF == 1 )
      call wrfparflowadvance(PF_TIME, PFDT, PF_FLUX, PF_PRESSURE, PF_POROSITY,    &
     &     PF_SATURATION, num_soil_layers, GHOST_SIZE_I_LOWER, GHOST_SIZE_J_LOWER,&
     &     GHOST_SIZE_I_UPPER, GHOST_SIZE_J_UPPER)
#else
      call wrf_error_fatal ( 'WRF is not compiled to run with ParFlow.' )
#endif

      END SUBROUTINE parflow_run

!------------------------------------------------------------------

END MODULE module_parflow
