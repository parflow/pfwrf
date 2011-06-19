module da_4dvar

use da_tracing, only : da_trace_entry, da_trace_exit
use da_reporting, only : da_error
use da_control, only : comm, var4d_bin, var4d_lbc, trace_use_dull, num_fgat_time

#ifdef VAR4D

use module_io_domain, only : open_r_dataset, input_input, close_dataset
use module_wrf_top, only : domain, head_grid, model_config_rec, config_flags, &
             wrf_init, wrf_run, wrf_run_tl, wrf_run_ad, wrf_finalize, &
             Setup_Timekeeping
use mediation_pertmod_io, only : xtraj_io_initialize, adtl_initialize, &
             save_ad_forcing, read_ad_forcing, read_nl_xtraj, save_tl_pert, &
             read_tl_pert, swap_ad_forcing
use module_configure, only :  model_to_grid_config_rec, grid_config_rec_type
use module_big_step_utilities_em, only : calc_mu_uv
use g_module_big_step_utilities_em, only : g_calc_mu_uv
use a_module_big_step_utilities_em, only : a_calc_mu_uv

#ifdef DM_PARALLEL
use module_dm, only : local_communicator
#endif

type (domain), pointer :: model_grid
type (grid_config_rec_type) :: model_config_flags

character*256 :: timestr

! Define some variables to save the NL physical option
integer :: original_mp_physics, original_ra_lw_physics, original_ra_sw_physics, &
           original_sf_sfclay_physics, original_bl_pbl_physics, original_cu_physics, &
           original_mp_zero_out, original_sf_surface_physics, original_ifsnow, &
           original_icloud, original_isfflx

REAL , DIMENSION(:,:,:) , ALLOCATABLE  :: ubdy3dtemp1 , vbdy3dtemp1 , tbdy3dtemp1 , pbdy3dtemp1 , qbdy3dtemp1
REAL , DIMENSION(:,:,:) , ALLOCATABLE  :: ubdy3dtemp2 , vbdy3dtemp2 , tbdy3dtemp2 , pbdy3dtemp2 , qbdy3dtemp2
REAL , DIMENSION(:,:,:) , ALLOCATABLE  :: mbdy2dtemp1,  mbdy2dtemp2

contains

#include "da_nl_model.inc"
#include "da_tl_model.inc"
#include "da_ad_model.inc"
#include "da_finalize_model.inc"
#include "da_4dvar_io.inc"
#include "da_4dvar_lbc.inc"

#endif

end module da_4dvar
