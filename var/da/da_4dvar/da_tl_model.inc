subroutine da_tl_model ()

   !-----------------------------------------------------------------------
   ! Purpose: Acrivate tangent linear  model
   !    Author: Xin Zhang,  MMM/ESSL/NCAR,  Date: 05/19/2010
   !-----------------------------------------------------------------------

   implicit none

   integer :: time_step, fid, ierr, i, j
   character (len=80)     :: inpname 

   if (trace_use_dull) call da_trace_entry("da_tl_model")

#ifdef _OPENMP
   call setfeenv()
#endif

   ! Set up perturbation output at each slots
   model_config_rec%auxhist8_outname = "tl_d<domain>_<date>"
   call nl_set_auxhist8_interval_s ( head_grid%id, var4d_bin ) 
   call nl_set_auxhist8_begin_s ( head_grid%id, var4d_bin )
   call nl_set_auxhist8_end_h ( head_grid%id, 999 )
   call nl_set_io_form_auxhist8 ( head_grid%id, 2 )
   call nl_set_frames_per_auxhist8 ( head_grid%id, 1 )


   ! Set up basic states reading
   model_config_rec%auxinput6_inname = "auxhist6_d<domain>_<date>"
   call nl_get_time_step ( head_grid%id, time_step )
   call nl_set_auxinput6_interval_s (head_grid%id, time_step )
   call nl_set_io_form_auxinput6 ( head_grid%id, 2 )
   call nl_set_frames_per_auxinput6 ( head_grid%id, 1 )

   ! Set all perturbation to ZERO

   head_grid%g_scalar = 0.0
   head_grid%g_tke_2 = 0.0
   head_grid%g_p = 0.0
   head_grid%g_al = 0.0
   head_grid%g_ustm = 0.0
   head_grid%g_hfx = 0.0
   head_grid%g_qfx = 0.0
   head_grid%g_h_diabatic = 0.0

   head_grid%g_u_1 = 0.0
   head_grid%g_v_1 = 0.0
   head_grid%g_w_1 = 0.0
   head_grid%g_t_1 = 0.0
   head_grid%g_ph_1 = 0.0
   head_grid%g_mu_1 = 0.0
   head_grid%g_tke_1 = 0.0

   head_grid%g_ru = 0.0
   head_grid%g_rv = 0.0
   head_grid%g_rw = 0.0
   head_grid%g_ww = 0.0
   head_grid%g_alt = 0.0
   head_grid%g_php = 0.0
   head_grid%g_mut = 0.0
   head_grid%g_muu = 0.0
   head_grid%g_muv = 0.0
   head_grid%g_p_hyd_w = 0.0
   head_grid%g_p_hyd = 0.0
   head_grid%g_z_at_w = 0.0
   head_grid%g_z = 0.0
   head_grid%g_zx = 0.0
   head_grid%g_zy = 0.0
   head_grid%g_rdz = 0.0
   head_grid%g_rdzw = 0.0
   head_grid%g_div = 0.0
   head_grid%g_nba_mij = 0.0
   head_grid%g_nba_rij = 0.0
   head_grid%g_defor11 = 0.0
   head_grid%g_defor22 = 0.0
   head_grid%g_defor33 = 0.0
   head_grid%g_defor12 = 0.0
   head_grid%g_defor13 = 0.0
   head_grid%g_defor23 = 0.0
   head_grid%g_bn2 = 0.0
   head_grid%g_xkmh = 0.0
   head_grid%g_xkmv = 0.0
   head_grid%g_xkhh = 0.0
   head_grid%g_xkhv = 0.0
   head_grid%g_ru_tend = 0.0
   head_grid%g_rv_tend = 0.0
   head_grid%g_u_save = 0.0
   head_grid%g_v_save = 0.0
   head_grid%g_t_save = 0.0
   head_grid%g_rthften = 0.0
   head_grid%g_rqvften = 0.0
   head_grid%g_muts = 0.0
   head_grid%g_mudf = 0.0
   head_grid%g_ru_m = 0.0
   head_grid%g_rv_m = 0.0
   head_grid%g_ww_m = 0.0

   if ( .not. var4d_lbc ) then
      head_grid%g_u_btxs = 0.0; head_grid%g_u_btxe = 0.0; head_grid%g_u_btys = 0.0; head_grid%g_u_btye = 0.0
      head_grid%g_v_btxs = 0.0; head_grid%g_v_btxe = 0.0; head_grid%g_v_btys = 0.0; head_grid%g_v_btye = 0.0
      head_grid%g_w_btxs = 0.0; head_grid%g_w_btxe = 0.0; head_grid%g_w_btys = 0.0; head_grid%g_w_btye = 0.0
      head_grid%g_t_btxs = 0.0; head_grid%g_t_btxe = 0.0; head_grid%g_t_btys = 0.0; head_grid%g_t_btye = 0.0
      head_grid%g_ph_btxs = 0.0; head_grid%g_ph_btxe = 0.0; head_grid%g_ph_btys = 0.0; head_grid%g_ph_btye = 0.0
      head_grid%g_mu_btxs = 0.0; head_grid%g_mu_btxe = 0.0; head_grid%g_mu_btys = 0.0; head_grid%g_mu_btye = 0.0
      head_grid%g_moist_btxs = 0.0; head_grid%g_moist_btxe = 0.0; head_grid%g_moist_btys = 0.0; head_grid%g_moist_btye = 0.0
   end if

   ! Set the physics options to what they are in TL and AD
   call nl_set_mp_physics (head_grid%id, 0) 
   call nl_set_ra_lw_physics (head_grid%id, 0)
   call nl_set_ra_sw_physics (head_grid%id, 0)
   call nl_set_sf_sfclay_physics (head_grid%id, 0)
   call nl_set_bl_pbl_physics (head_grid%id, 0)
   call nl_set_cu_physics (head_grid%id, 0)
   call nl_set_mp_zero_out (head_grid%id, 0)
   call nl_set_sf_surface_physics (head_grid%id, 1)
   call nl_set_ifsnow (head_grid%id, 1)
   call nl_set_icloud (head_grid%id, 0)
   call nl_set_isfflx (head_grid%id, 0)

   ! WRF model time-stepping.  Calls integrate().
   call wrf_run_tl
	
   ! restore the physics options to the originals
   call nl_set_mp_physics (head_grid%id, original_mp_physics) 
   call nl_set_ra_lw_physics (head_grid%id, original_ra_lw_physics)
   call nl_set_ra_sw_physics (head_grid%id, original_ra_sw_physics)
   call nl_set_sf_sfclay_physics (head_grid%id, original_sf_sfclay_physics)
   call nl_set_bl_pbl_physics (head_grid%id, original_bl_pbl_physics)
   call nl_set_cu_physics (head_grid%id, original_cu_physics)
   call nl_set_mp_zero_out (head_grid%id, original_mp_zero_out)
   call nl_set_sf_surface_physics (head_grid%id, original_sf_surface_physics)
   call nl_set_ifsnow (head_grid%id, original_ifsnow)
   call nl_set_icloud (head_grid%id, original_icloud)
   call nl_set_isfflx (head_grid%id, original_isfflx)

   ! Turn off auxinput2 reading and auxhist8 output

   call nl_set_auxinput6_interval_s (head_grid%id, 0 )
   call nl_set_auxhist8_interval_s ( head_grid%id, 0 )

   call Setup_Timekeeping ( head_grid )

   if (trace_use_dull) call da_trace_exit("da_tl_model")

end subroutine da_tl_model


