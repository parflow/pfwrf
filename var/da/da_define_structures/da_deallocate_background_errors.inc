subroutine da_deallocate_background_errors (be)

   !---------------------------------------------------------------------------
   ! Purpose: Deallocate components of wrfvar background errors.
   !
   !  Update: Multivariate BE option (cv_options=6)
   !          Syed RH Rizvi (MMM/NESL/NCAR)   Date: 02/01/2010
   !
   !  Note: Please acknowledge author/institute in work that uses this code.
   !---------------------------------------------------------------------------

   implicit none

   type (be_type), intent(inout)        :: be     ! Background error structure.
   
   if (trace_use) call da_trace_entry("da_deallocate_background_errors")

   if (cv_options /= 3) then

      ! Deallocate gridpoint errors:

      if (be % v1 % mz > 0) deallocate (be % v1 % val)
      if (be % v2 % mz > 0) deallocate (be % v2 % val)
      if (be % v3 % mz > 0) deallocate (be % v3 % val)
      if (be % v4 % mz > 0) deallocate (be % v4 % val)
      if (be % v5 % mz > 0 .and. .not. global) deallocate (be % v5 % val) 
#ifdef CLOUD_CV
      if (be % v6 % mz > 0) deallocate (be % v6 % val)
      if (be % v7 % mz > 0) deallocate (be % v7 % val)
      if (be % v8 % mz > 0) deallocate (be % v8 % val)
#endif    
      if (be % v1 % mz > 0) deallocate (be % v1 % rf_alpha)
      if (be % v2 % mz > 0) deallocate (be % v2 % rf_alpha)
      if (be % v3 % mz > 0) deallocate (be % v3 % rf_alpha)
      if (be % v4 % mz > 0) deallocate (be % v4 % rf_alpha)
      if (be % v5 % mz > 0 .and. .not. global) deallocate (be % v5 % rf_alpha)
#ifdef CLOUD_CV
      if (be % v6 % mz > 0) deallocate (be % v6 % rf_alpha)
      if (be % v7 % mz > 0) deallocate (be % v7 % rf_alpha)
      if (be % v8 % mz > 0) deallocate (be % v8 % rf_alpha)
#endif
      if (global) then
         if (be % v1 % mz > 0) deallocate (be % v1 % power)
         if (be % v2 % mz > 0) deallocate (be % v2 % power)
         if (be % v3 % mz > 0) deallocate (be % v3 % power)
         if (be % v4 % mz > 0) deallocate (be % v4 % power)
         if (be % v5 % mz > 0) deallocate (be % v5 % power) 
      end if

      ! Deallocate eigenvectors of vertical error covariance:

      if (vert_corr == vert_corr_2) then
         if (be % v1 % mz > 0) deallocate (be % v1 % evec)
         if (be % v2 % mz > 0) deallocate (be % v2 % evec)
         if (be % v3 % mz > 0) deallocate (be % v3 % evec)
         if (be % v4 % mz > 0) deallocate (be % v4 % evec)
         if (be % v5 % mz > 0 .and. .not. global) deallocate (be % v5 % evec)
#ifdef CLOUD_CV
         if (be % v6 % mz > 0) deallocate (be % v6 % evec)
         if (be % v7 % mz > 0) deallocate (be % v7 % evec)
         if (be % v8 % mz > 0) deallocate (be % v8 % evec)
#endif
         if (be % v1 % mz > 0) deallocate (be % v1 % evec_g)
         if (be % v2 % mz > 0) deallocate (be % v2 % evec_g)
         if (be % v3 % mz > 0) deallocate (be % v3 % evec_g)
         if (be % v4 % mz > 0) deallocate (be % v4 % evec_g)
         if (be % v5 % mz > 0 .and. .not. global) deallocate (be % v5 % evec_g)
#ifdef CLOUD_CV
         if (be % v6 % mz > 0) deallocate (be % v6 % evec_g)
         if (be % v7 % mz > 0) deallocate (be % v7 % evec_g)
         if (be % v8 % mz > 0) deallocate (be % v8 % evec_g)
#endif
         if (be % v1 % mz > 0) deallocate (be % v1 % val_g)
         if (be % v2 % mz > 0) deallocate (be % v2 % val_g)
         if (be % v3 % mz > 0) deallocate (be % v3 % val_g)
         if (be % v4 % mz > 0) deallocate (be % v4 % val_g)
         if (be % v5 % mz > 0 .and. .not. global) deallocate (be % v5 % val_g)
#ifdef CLOUD_CV
         if (be % v6 % mz > 0) deallocate (be % v6 % val_g)
         if (be % v7 % mz > 0) deallocate (be % v7 % val_g)
         if (be % v8 % mz > 0) deallocate (be % v8 % val_g)
#endif
      end if

      deallocate (be % reg_psi_chi)
      deallocate (be % reg_psi_t)
      deallocate (be % reg_psi_ps)
      deallocate (be % reg_psi_rh)
      deallocate (be % reg_chi_u_t)
      deallocate (be % reg_chi_u_ps)
      deallocate (be % reg_chi_u_rh)
      deallocate (be % reg_t_u_rh)
      deallocate (be % reg_ps_u_rh)

      ! Deallocate control variable errors (in future uncomment use these to allow 
      ! eg NMC error correlations).

      ! deallocate (be % cv % val)

   else ! for cv_options = 3
    
      deallocate (be % corz)
      deallocate (be % corp)
      deallocate (be % vz)
      deallocate (be % agvz)
      deallocate (be % bvz)
      deallocate (be % wgvz)
      deallocate (be % be)
      deallocate (be % rate)
      deallocate (be % table)
      deallocate (be % slix)
      deallocate (be % slipx)
      deallocate (be % sljy)
      deallocate (be % sljpy)
   
   end if

   ! Deallocate wavelet parameters:
   if( .not. use_rf )deallocate(be%wsd,ws)
   if( do_normalize )deallocate(be%sd)
   if( do_normalize .or. .not. use_rf )deallocate(nij)

   if (trace_use) call da_trace_exit("da_deallocate_background_errors")

end subroutine da_deallocate_background_errors


