subroutine da_transfer_wrftltoxa(grid, config_flags, filnam, timestr)

   !---------------------------------------------------------------------------
   ! Purpose: Convert WRFTL variables to analysis increments
   !           (inverse of the incremental part of xatowrf)
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain),               intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   character*4,                intent(in)    :: filnam
   character*256,              intent(in)    :: timestr

#ifdef VAR4D

   integer :: i, j, k
   integer :: is, ie, js, je, ks, ke

   real :: sdmd, s1md
   real, allocatable, dimension(:,:,:) :: g_press
   real, allocatable, dimension(:,:) :: ptmp

   integer ndynopt

   if (trace_use) call da_trace_entry("da_transfer_wrftltoxa")

   !---------------------------------------------------------------------------
   ! [0.0] input
   !---------------------------------------------------------------------------

   is=grid%xp%its
   ie=grid%xp%ite
   js=grid%xp%jts
   je=grid%xp%jte
   ks=grid%xp%kts
   ke=grid%xp%kte

   allocate (g_press(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme,grid%xp%kms:grid%xp%kme))


   call pop_tl_pert (timestr)

   call kj_swap_reverse (model_grid%g_u_2, grid%g_u_2, &
                 grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   call kj_swap_reverse (model_grid%g_v_2, grid%g_v_2, &
                 grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   call kj_swap_reverse (model_grid%g_w_2, grid%g_w_2, &
                 grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   call kj_swap_reverse (model_grid%g_t_2, grid%g_t_2, &
                 grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   call kj_swap_reverse (model_grid%g_ph_2, grid%g_ph_2, &
                grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   grid%g_mu_2 = model_grid%g_mu_2

   do i = PARAM_FIRST_SCALAR, num_moist
      call kj_swap_reverse (model_grid%g_moist(:,:,:,i), grid%g_moist(:,:,:,i), &
                    grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   enddo

   !---------------------------------------------------------------------------
   ! [1.0] Get the specific humidity increments from mixing ratio increments
   !---------------------------------------------------------------------------

   do k=ks,ke
      do j=js,je
         do i=is,ie
            grid%xa%q(i,j,k)=grid%g_moist(i,j,k,P_G_QV)*(1.0-grid%xb%q(i,j,k))**2
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [2.0] COMPUTE psfc increments from mu-increments
   !---------------------------------------------------------------------------

   do j=js,je
      do i=is,ie
         sdmd=0.0
         s1md=0.0
         do k=ks,ke
            sdmd=sdmd+grid%g_moist(i,j,k,P_G_QV)*grid%dnw(k)
            s1md=s1md+(1.0+grid%moist(i,j,k,P_QV))*grid%dnw(k)
         end do
         grid%xa%psfc(i,j)=-grid%xb%psac(i,j)*sdmd-grid%g_mu_2(i,j)*s1md
      end do
   end do

   !---------------------------------------------------------------------------
   ! [3.0] COMPUTE pressure increments 
   !---------------------------------------------------------------------------

   do j=js,je
      do i=is,ie
         g_press(i,j,ke+1)=0.0
         do k=ke,ks,-1
            g_press(i,j,k)=g_press(i,j,k+1) &
               -(grid%g_mu_2(i,j)*(1.0+grid%moist(i,j,k,P_QV)) &
               +(grid%mu_2(i,j)+grid%mub(i,j))* &
               grid%g_moist(i,j,k,P_G_QV))*grid%dn(k)
            grid%xa%p(i,j,k)=0.5*(g_press(i,j,k)+g_press(i,j,k+1))
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [4.0] convert theta increments to t increments
   !---------------------------------------------------------------------------

   do k=ks,ke
      do j=js,je
         do i=is,ie
            grid%xa%t(i,j,k)=grid%xb%t(i,j,k)*(grid%g_t_2(i,j,k)/ &
               (t0+grid%t_2(i,j,k)) &
               +kappa*grid%xa%p(i,j,k)/grid%xb%p(i,j,k))
         end do
      end do
   end do

   deallocate (g_press)

   ! FIX? In the inverse, g_ph information is lost. This should be investigated 
   ! later.

   !-------------------------------------------------------------------------
   ! [5.0] convert from c-grid to a-grid
   !-------------------------------------------------------------------------

#ifdef DM_PARALLEL
#include "HALO_EM_C_TL.inc"
#endif

   do k=ks,ke
      do j=js,je
         do i=is,ie
            grid%xa%u(i,j,k)=0.5*(grid%g_u_2(i+1,j,  k)+grid%g_u_2(i,j,k))
            grid%xa%v(i,j,k)=0.5*(grid%g_v_2(i  ,j+1,k)+grid%g_v_2(i,j,k))
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [6.0] all the simple ones
   !---------------------------------------------------------------------------

   do k=ks,ke+1
      do j=js,je
         do i=is,ie
            grid%xa%w(i,j,k)=grid%g_w_2(i,j,k)
         end do
      end do
   end do

   if(var4d)then
    if (size(grid%g_moist,dim=4) >= 4) then
      do k=ks,ke
         do j=js,je
            do i=is,ie
               grid%xa%qcw(i,j,k)=grid%g_moist(i,j,k,p_g_qc)
               grid%xa%qrn(i,j,k)=grid%g_moist(i,j,k,p_g_qr)
            end do
         end do
      end do
    end if
   endif

#ifdef VAR4D_MICROPHYSICS
   ! New code needed once we introduce the microphysics to 4dvar in 2008
   if (size(grid%moist,dim=4) >= 4) then
      do k=ks,ke
         do j=js,je
            do i=is,ie
               grid%xa%qcw(i,j,k)=grid%g_moist(i,j,k,p_qcw)
               grid%xa%qrn(i,j,k)=grid%g_moist(i,j,k,p_qrn)
            end do
         end do
      end do
   end if

   if (size(grid%moist,dim=4) >= 6) then
      do k=ks,ke
         do j=js,je
            do i=is,ie
               grid%xa%qci(i,j,k)=grid%g_moist(i,j,k,p_qci)
               grid%xa%qsn(i,j,k)=grid%g_moist(i,j,k,p_qsn)
            end do
         end do
      end do
   end if

   if (size(grid%moist_2,dim=4) >= 7) then
      do k=ks,ke
         do j=js,je
            do i=is,ie
               grid%xa%qgr(i,j,k)=grid%g_moist(i,j,k,p_qgr) 
            end do
         end do
      end do
   end if
#endif

#ifdef DM_PARALLEL
#include "HALO_XA.inc"
#endif

   if (trace_use) call da_trace_exit("da_transfer_wrftltoxa")

#endif
end subroutine da_transfer_wrftltoxa


