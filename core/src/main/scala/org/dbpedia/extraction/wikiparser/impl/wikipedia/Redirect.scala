package org.dbpedia.extraction.wikiparser.impl.wikipedia

import org.dbpedia.extraction.util.Language

/**
 * Holds the redirect identifiers of the different Wikipedia languages. Do not edit Redirect.scala!
 * GenerateWikiConfig.scala uses the template file Redirect.scala.txt to generate Redirect.scala.
 */
object Redirect
{
    private val map : Map[String, Set[String]] =
    Map("mappings"->mappings_redirects,"commons"->commons_redirects,"aa"->aa_redirects,"ab"->ab_redirects,"ace"->ace_redirects,"af"->af_redirects,"ak"->ak_redirects,"als"->als_redirects,"am"->am_redirects,"an"->an_redirects,"ang"->ang_redirects,"ar"->ar_redirects,"arc"->arc_redirects,"arz"->arz_redirects,"as"->as_redirects,"ast"->ast_redirects,"av"->av_redirects,"ay"->ay_redirects,"az"->az_redirects,"ba"->ba_redirects,"bar"->bar_redirects,"bat-smg"->bat_smg_redirects,"bcl"->bcl_redirects,"be"->be_redirects,"be-x-old"->be_x_old_redirects,"bg"->bg_redirects,"bh"->bh_redirects,"bi"->bi_redirects,"bjn"->bjn_redirects,"bm"->bm_redirects,"bn"->bn_redirects,"bo"->bo_redirects,"bpy"->bpy_redirects,"br"->br_redirects,"bs"->bs_redirects,"bug"->bug_redirects,"bxr"->bxr_redirects,"ca"->ca_redirects,"cbk-zam"->cbk_zam_redirects,"cdo"->cdo_redirects,"ce"->ce_redirects,"ceb"->ceb_redirects,"ch"->ch_redirects,"cho"->cho_redirects,"chr"->chr_redirects,"chy"->chy_redirects,"ckb"->ckb_redirects,"co"->co_redirects,"cr"->cr_redirects,"crh"->crh_redirects,"cs"->cs_redirects,"csb"->csb_redirects,"cu"->cu_redirects,"cv"->cv_redirects,"cy"->cy_redirects,"da"->da_redirects,"de"->de_redirects,"diq"->diq_redirects,"dsb"->dsb_redirects,"dv"->dv_redirects,"dz"->dz_redirects,"ee"->ee_redirects,"el"->el_redirects,"eml"->eml_redirects,"en"->en_redirects,"eo"->eo_redirects,"es"->es_redirects,"et"->et_redirects,"eu"->eu_redirects,"ext"->ext_redirects,"fa"->fa_redirects,"ff"->ff_redirects,"fi"->fi_redirects,"fiu-vro"->fiu_vro_redirects,"fj"->fj_redirects,"fo"->fo_redirects,"fr"->fr_redirects,"frp"->frp_redirects,"frr"->frr_redirects,"fur"->fur_redirects,"fy"->fy_redirects,"ga"->ga_redirects,"gag"->gag_redirects,"gan"->gan_redirects,"gd"->gd_redirects,"gl"->gl_redirects,"glk"->glk_redirects,"gn"->gn_redirects,"got"->got_redirects,"gu"->gu_redirects,"gv"->gv_redirects,"ha"->ha_redirects,"hak"->hak_redirects,"haw"->haw_redirects,"he"->he_redirects,"hi"->hi_redirects,"hif"->hif_redirects,"ho"->ho_redirects,"hr"->hr_redirects,"hsb"->hsb_redirects,"ht"->ht_redirects,"hu"->hu_redirects,"hy"->hy_redirects,"hz"->hz_redirects,"ia"->ia_redirects,"id"->id_redirects,"ie"->ie_redirects,"ig"->ig_redirects,"ii"->ii_redirects,"ik"->ik_redirects,"ilo"->ilo_redirects,"io"->io_redirects,"is"->is_redirects,"it"->it_redirects,"iu"->iu_redirects,"ja"->ja_redirects,"jbo"->jbo_redirects,"jv"->jv_redirects,"ka"->ka_redirects,"kaa"->kaa_redirects,"kab"->kab_redirects,"kbd"->kbd_redirects,"kg"->kg_redirects,"ki"->ki_redirects,"kj"->kj_redirects,"kk"->kk_redirects,"kl"->kl_redirects,"km"->km_redirects,"kn"->kn_redirects,"ko"->ko_redirects,"koi"->koi_redirects,"kr"->kr_redirects,"krc"->krc_redirects,"ks"->ks_redirects,"ksh"->ksh_redirects,"ku"->ku_redirects,"kv"->kv_redirects,"kw"->kw_redirects,"ky"->ky_redirects,"la"->la_redirects,"lad"->lad_redirects,"lb"->lb_redirects,"lbe"->lbe_redirects,"lez"->lez_redirects,"lg"->lg_redirects,"li"->li_redirects,"lij"->lij_redirects,"lmo"->lmo_redirects,"ln"->ln_redirects,"lo"->lo_redirects,"lt"->lt_redirects,"ltg"->ltg_redirects,"lv"->lv_redirects,"map-bms"->map_bms_redirects,"mdf"->mdf_redirects,"mg"->mg_redirects,"mh"->mh_redirects,"mhr"->mhr_redirects,"mi"->mi_redirects,"mk"->mk_redirects,"ml"->ml_redirects,"mn"->mn_redirects,"mo"->mo_redirects,"mr"->mr_redirects,"mrj"->mrj_redirects,"ms"->ms_redirects,"mt"->mt_redirects,"mus"->mus_redirects,"mwl"->mwl_redirects,"my"->my_redirects,"myv"->myv_redirects,"mzn"->mzn_redirects,"na"->na_redirects,"nah"->nah_redirects,"nap"->nap_redirects,"nds"->nds_redirects,"nds-nl"->nds_nl_redirects,"ne"->ne_redirects,"new"->new_redirects,"ng"->ng_redirects,"nl"->nl_redirects,"nn"->nn_redirects,"no"->no_redirects,"nov"->nov_redirects,"nrm"->nrm_redirects,"nso"->nso_redirects,"nv"->nv_redirects,"ny"->ny_redirects,"oc"->oc_redirects,"om"->om_redirects,"or"->or_redirects,"os"->os_redirects,"pa"->pa_redirects,"pag"->pag_redirects,"pam"->pam_redirects,"pap"->pap_redirects,"pcd"->pcd_redirects,"pdc"->pdc_redirects,"pfl"->pfl_redirects,"pi"->pi_redirects,"pih"->pih_redirects,"pl"->pl_redirects,"pms"->pms_redirects,"pnt"->pnt_redirects,"pnb"->pnb_redirects,"ps"->ps_redirects,"pt"->pt_redirects,"qu"->qu_redirects,"rm"->rm_redirects,"rmy"->rmy_redirects,"rn"->rn_redirects,"ro"->ro_redirects,"roa-rup"->roa_rup_redirects,"roa-tara"->roa_tara_redirects,"ru"->ru_redirects,"rue"->rue_redirects,"rw"->rw_redirects,"sa"->sa_redirects,"sah"->sah_redirects,"sc"->sc_redirects,"scn"->scn_redirects,"sco"->sco_redirects,"sd"->sd_redirects,"se"->se_redirects,"sg"->sg_redirects,"sh"->sh_redirects,"si"->si_redirects,"simple"->simple_redirects,"sk"->sk_redirects,"sl"->sl_redirects,"sm"->sm_redirects,"sn"->sn_redirects,"so"->so_redirects,"sq"->sq_redirects,"sr"->sr_redirects,"srn"->srn_redirects,"ss"->ss_redirects,"st"->st_redirects,"stq"->stq_redirects,"su"->su_redirects,"sv"->sv_redirects,"sw"->sw_redirects,"szl"->szl_redirects,"ta"->ta_redirects,"te"->te_redirects,"tet"->tet_redirects,"tg"->tg_redirects,"th"->th_redirects,"ti"->ti_redirects,"tk"->tk_redirects,"tl"->tl_redirects,"tn"->tn_redirects,"to"->to_redirects,"tpi"->tpi_redirects,"tr"->tr_redirects,"ts"->ts_redirects,"tt"->tt_redirects,"tum"->tum_redirects,"tw"->tw_redirects,"ty"->ty_redirects,"udm"->udm_redirects,"ug"->ug_redirects,"uk"->uk_redirects,"ur"->ur_redirects,"uz"->uz_redirects,"ve"->ve_redirects,"vec"->vec_redirects,"vep"->vep_redirects,"vi"->vi_redirects,"vls"->vls_redirects,"vo"->vo_redirects,"wa"->wa_redirects,"war"->war_redirects,"wo"->wo_redirects,"wuu"->wuu_redirects,"xal"->xal_redirects,"xh"->xh_redirects,"xmf"->xmf_redirects,"yi"->yi_redirects,"yo"->yo_redirects,"za"->za_redirects,"zea"->zea_redirects,"zh"->zh_redirects,"zh-classical"->zh_classical_redirects,"zh-min-nan"->zh_min_nan_redirects,"zh-yue"->zh_yue_redirects,"zu"->zu_redirects,"cz"->cs_redirects,"dk"->da_redirects,"epo"->eo_redirects,"jp"->ja_redirects,"minnan"->zh_min_nan_redirects,"nan"->zh_min_nan_redirects,"nb"->no_redirects,"zh-cfr"->zh_min_nan_redirects)
    private def mappings_redirects = Set("#REDIRECT")
    private def commons_redirects = Set("#REDIRECT")
    private def aa_redirects = Set("#REDIRECT")
    private def ab_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def ace_redirects = Set("#ALIH","#REDIRECT")
    private def af_redirects = Set("#AANSTUUR","#REDIRECT")
    private def ak_redirects = Set("#REDIRECT")
    private def als_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def am_redirects = Set("#REDIRECT")
    private def an_redirects = Set("#ENDRECERA","#REENDRECERA","#REDIRECCIÓN","#REDIRECCION","#REDIRECT")
    private def ang_redirects = Set("#REDIRECT")
    private def ar_redirects = Set("#تحويل","#REDIRECT")
    private def arc_redirects = Set("#ܨܘܝܒܐ","#REDIRECT")
    private def arz_redirects = Set("#تحويل","#REDIRECT")
    private def as_redirects = Set("পুণঃনিৰ্দেশ","#REDIRECT")
    private def ast_redirects = Set("#REDIRECT")
    private def av_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def ay_redirects = Set("#REDIRECCIÓN","#REDIRECCION","#REDIRECT")
    private def az_redirects = Set("#İSTİQAMƏTLƏNDİRMƏ","#İSTİQAMƏTLƏNDİR","#REDIRECT")
    private def ba_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def bar_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def bat_smg_redirects = Set("#PERADRESAVIMAS","#REDIRECT")
    private def bcl_redirects = Set("#REDIRECT")
    private def be_redirects = Set("#REDIRECT")
    private def be_x_old_redirects = Set("#перанакіраваньне","#REDIRECT")
    private def bg_redirects = Set("#пренасочване","#виж","#REDIRECT")
    private def bh_redirects = Set("#REDIRECT")
    private def bi_redirects = Set("#REDIRECT")
    private def bjn_redirects = Set("#ALIH","#REDIRECT")
    private def bm_redirects = Set("#REDIRECTION","#REDIRECT")
    private def bn_redirects = Set("#REDIRECT")
    private def bo_redirects = Set("#REDIRECT")
    private def bpy_redirects = Set("#REDIRECT")
    private def br_redirects = Set("#ADKAS","#REDIRECT")
    private def bs_redirects = Set("#PREUSMJERI","#REDIRECT")
    private def bug_redirects = Set("#ALIH","#REDIRECT")
    private def bxr_redirects = Set("#REDIRECT")
    private def ca_redirects = Set("#REDIRECT")
    private def cbk_zam_redirects = Set("#REDIRECCIÓN","#REDIRECCION","#REDIRECT")
    private def cdo_redirects = Set("#REDIRECT")
    private def ce_redirects = Set("#дlасахьажайар'","'#хьажайо'","'#REDIRECT","#перенаправление","#перенапр","#REDIRECT")
    private def ceb_redirects = Set("#REDIRECT")
    private def ch_redirects = Set("#REDIRECT")
    private def cho_redirects = Set("#REDIRECT")
    private def chr_redirects = Set("#REDIRECT")
    private def chy_redirects = Set("#REDIRECT")
    private def ckb_redirects = Set("#REDIRECT")
    private def co_redirects = Set("#REDIRECT")
    private def cr_redirects = Set("#REDIRECT")
    private def crh_redirects = Set("#REDIRECT")
    private def cs_redirects = Set("#PŘESMĚRUJ","#REDIRECT")
    private def csb_redirects = Set("#PATRZ","#PRZEKIERUJ","#TAM","#REDIRECT")
    private def cu_redirects = Set("#ПРѢНАПРАВЛЄНИѤ","#REDIRECT")
    private def cv_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def cy_redirects = Set("#ail-cyfeirio","#ailgyfeirio","#REDIRECT")
    private def da_redirects = Set("#REDIRECT")
    private def de_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def diq_redirects = Set("#HETENAYIŞ","#REDIRECT")
    private def dsb_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def dv_redirects = Set("#REDIRECT")
    private def dz_redirects = Set("#REDIRECT")
    private def ee_redirects = Set("#REDIRECT")
    private def el_redirects = Set("#ΑΝΑΚΑΤΕΥΘΥΝΣΗ","#REDIRECT")
    private def eml_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def en_redirects = Set("#REDIRECT")
    private def eo_redirects = Set("#ALIDIREKTU","#ALIDIREKTI","#AL","#REDIRECT")
    private def es_redirects = Set("#REDIRECCIÓN","#REDIRECCION","#REDIRECT")
    private def et_redirects = Set("#suuna","#REDIRECT")
    private def eu_redirects = Set("#BIRZUZENDU","#REDIRECT")
    private def ext_redirects = Set("#REDIRECT")
    private def fa_redirects = Set("#تغییر_مسیر","#تغییرمسیر","#REDIRECT")
    private def ff_redirects = Set("#REDIRECTION","#REDIRECT")
    private def fi_redirects = Set("#OHJAUS","#UUDELLEENOHJAUS","#REDIRECT")
    private def fiu_vro_redirects = Set("#saadaq","#suuna","#REDIRECT")
    private def fj_redirects = Set("#REDIRECT")
    private def fo_redirects = Set("#REDIRECT")
    private def fr_redirects = Set("#REDIRECTION","#REDIRECT")
    private def frp_redirects = Set("#REDIRÈCCION","#REDIRECTION","#REDIRECT")
    private def frr_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def fur_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def fy_redirects = Set("#REDIRECT")
    private def ga_redirects = Set("#athsheoladh","#REDIRECT")
    private def gag_redirects = Set("#YÖNNENDİRMÄKLER","#YÖNNENDİR","#YÖNNENDİRMÄ","#YÖNLENDİRME","#YÖNLENDİR","#REDIRECT")
    private def gan_redirects = Set("#重定向","#REDIRECT")
    private def gd_redirects = Set("#REDIRECT")
    private def gl_redirects = Set("#REDIRECCIÓN","#REDIRECIONAMENTO","#REDIRECT")
    private def glk_redirects = Set("#تغییر_مسیر","#تغییرمسیر","#REDIRECT")
    private def gn_redirects = Set("#REDIRECCIÓN","#REDIRECCION","#REDIRECT")
    private def got_redirects = Set("#REDIRECT")
    private def gu_redirects = Set("#REDIRECT")
    private def gv_redirects = Set("#REDIRECT")
    private def ha_redirects = Set("#REDIRECT")
    private def hak_redirects = Set("#REDIRECT")
    private def haw_redirects = Set("#REDIRECT")
    private def he_redirects = Set("#הפניה","#REDIRECT")
    private def hi_redirects = Set("#REDIRECT")
    private def hif_redirects = Set("#REDIRECT")
    private def ho_redirects = Set("#REDIRECT")
    private def hr_redirects = Set("#PREUSMJERI","#REDIRECT")
    private def hsb_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def ht_redirects = Set("#REDIRECTION","#REDIRECT")
    private def hu_redirects = Set("#ÁTIRÁNYÍTÁS","#REDIRECT")
    private def hy_redirects = Set("#ՎԵՐԱՀՂՈՒՄ","#REDIRECT")
    private def hz_redirects = Set("#REDIRECT")
    private def ia_redirects = Set("#REDIRECT")
    private def id_redirects = Set("#ALIH","#REDIRECT")
    private def ie_redirects = Set("#REDIRECT")
    private def ig_redirects = Set("#KÚFÙ","#REDIRECT")
    private def ii_redirects = Set("#重定向","#REDIRECT")
    private def ik_redirects = Set("#REDIRECT")
    private def ilo_redirects = Set("#REDIRECT")
    private def io_redirects = Set("#REDIRECT")
    private def is_redirects = Set("#tilvísun","#TILVÍSUN","#REDIRECT")
    private def it_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def iu_redirects = Set("#REDIRECT")
    private def ja_redirects = Set("#転送","#リダイレクト","＃転送","＃リダイレクト","#REDIRECT")
    private def jbo_redirects = Set("#REDIRECT")
    private def jv_redirects = Set("#ALIH","#REDIRECT")
    private def ka_redirects = Set("#გადამისამართება","#REDIRECT")
    private def kaa_redirects = Set("#AÝDAW","#АЙДАУ","#REDIRECT")
    private def kab_redirects = Set("#REDIRECT")
    private def kbd_redirects = Set("#REDIRECT")
    private def kg_redirects = Set("#REDIRECT")
    private def ki_redirects = Set("#REDIRECT")
    private def kj_redirects = Set("#REDIRECT")
    private def kk_redirects = Set("#АЙДАУ","#REDIRECT")
    private def kl_redirects = Set("#REDIRECT")
    private def km_redirects = Set("#បញ្ជូនបន្ត","#ប្ដូរទីតាំងទៅ","#ប្តូរទីតាំងទៅ","#ប្ដូរទីតាំង","#ប្តូរទីតាំង","#ប្ដូរចំណងជើង","#REDIRECT")
    private def kn_redirects = Set("#REDIRECT")
    private def ko_redirects = Set("#넘겨주기","#REDIRECT")
    private def koi_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def kr_redirects = Set("#REDIRECT")
    private def krc_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def ks_redirects = Set("#REDIRECT")
    private def ksh_redirects = Set("#ÖMLEIDE_OP","#ÖMLEIDE","#LEIDT_ÖM_OP","#ÖMLEIDUNG","#WEITERLEITUNG","#REDIRECT")
    private def ku_redirects = Set("#BERALÎKIRIN","#REDIRECT")
    private def kv_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def kw_redirects = Set("#REDIRECT")
    private def ky_redirects = Set("#REDIRECT")
    private def la_redirects = Set("#REDIRECT")
    private def lad_redirects = Set("#DIRIJAR","#DIRECCIÓN","#REDIRECCIÓN","#REDIRECCION","#REDIRECT")
    private def lb_redirects = Set("#VIRULEEDUNG","#WEITERLEITUNG","#REDIRECT")
    private def lbe_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def lez_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def lg_redirects = Set("#REDIRECT")
    private def li_redirects = Set("#DOORVERWIJZING","#REDIRECT")
    private def lij_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def lmo_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def ln_redirects = Set("#REDIRECTION","#REDIRECT")
    private def lo_redirects = Set("#REDIRECT")
    private def lt_redirects = Set("#PERADRESAVIMAS","#REDIRECT")
    private def ltg_redirects = Set("#REDIRECT")
    private def lv_redirects = Set("#REDIRECT")
    private def map_bms_redirects = Set("#ALIH","#REDIRECT")
    private def mdf_redirects = Set("#REDIRECT")
    private def mg_redirects = Set("#FIHODINANA","#REDIRECTION","#REDIRECT")
    private def mh_redirects = Set("#REDIRECT")
    private def mhr_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def mi_redirects = Set("#REDIRECT")
    private def mk_redirects = Set("#пренасочување","#види","#Пренасочување","#ПРЕНАСОЧУВАЊЕ","#REDIRECT")
    private def ml_redirects = Set("#തിരിച്ചുവിടുക","#തിരിച്ചുവിടൽ","#REDIRECT")
    private def mn_redirects = Set("#REDIRECT")
    private def mo_redirects = Set("#REDIRECTEAZA","#REDIRECT")
    private def mr_redirects = Set("#पुनर्निर्देशन","#पुर्ननिर्देशन","#REDIRECT")
    private def mrj_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def ms_redirects = Set("#LENCONG","#REDIRECT")
    private def mt_redirects = Set("#RINDIRIZZA","#REDIRECT")
    private def mus_redirects = Set("#REDIRECT")
    private def mwl_redirects = Set("#ANCAMINAR","#REDIRECIONAMENTO","#REDIRECT")
    private def my_redirects = Set("#REDIRECT")
    private def myv_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def mzn_redirects = Set("#بور","#تغییرمسیر","#تغییر_مسیر","#REDIRECT")
    private def na_redirects = Set("#REDIRECT")
    private def nah_redirects = Set("#REDIRECCIÓN","#REDIRECCION","#REDIRECT")
    private def nap_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def nds_redirects = Set("#wiederleiden","#WEITERLEITUNG","#REDIRECT")
    private def nds_nl_redirects = Set("#DEURVERWIEZING","#DOORVERWIJZING","#REDIRECT")
    private def ne_redirects = Set("#REDIRECT")
    private def new_redirects = Set("#REDIRECT")
    private def ng_redirects = Set("#REDIRECT")
    private def nl_redirects = Set("#DOORVERWIJZING","#REDIRECT")
    private def nn_redirects = Set("#omdiriger","#REDIRECT")
    private def no_redirects = Set("#OMDIRIGERING","#REDIRECT")
    private def nov_redirects = Set("#REDIRECT")
    private def nrm_redirects = Set("#REDIRECT")
    private def nso_redirects = Set("#REDIRECT")
    private def nv_redirects = Set("#REDIRECT")
    private def ny_redirects = Set("#REDIRECT")
    private def oc_redirects = Set("#REDIRECCION","#REDIRECT")
    private def om_redirects = Set("#REDIRECT")
    private def or_redirects = Set("#ଲେଉଟାଣି","#REDIRECT")
    private def os_redirects = Set("#РАРВЫСТ","#перенаправление","#перенапр","#REDIRECT")
    private def pa_redirects = Set("#REDIRECT")
    private def pag_redirects = Set("#REDIRECT")
    private def pam_redirects = Set("#REDIRECT")
    private def pap_redirects = Set("#REDIRECT")
    private def pcd_redirects = Set("#REDIRECTION","#REDIRECT")
    private def pdc_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def pfl_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def pi_redirects = Set("#REDIRECT")
    private def pih_redirects = Set("#REDIRECT")
    private def pl_redirects = Set("#PATRZ","#PRZEKIERUJ","#TAM","#REDIRECT")
    private def pms_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def pnt_redirects = Set("#REDIRECT")
    private def pnb_redirects = Set("#REDIRECT")
    private def ps_redirects = Set("#REDIRECT")
    private def pt_redirects = Set("#REDIRECIONAMENTO","#REDIRECT")
    private def qu_redirects = Set("#PUSAPUNA","#REDIRECCIÓN","#REDIRECCION","#REDIRECT")
    private def rm_redirects = Set("#RENVIAMENT","#REDIRECT")
    private def rmy_redirects = Set("#REDIRECTEAZA","#REDIRECT")
    private def rn_redirects = Set("#REDIRECT")
    private def ro_redirects = Set("#REDIRECTEAZA","#REDIRECT")
    private def roa_rup_redirects = Set("#REDIRECT")
    private def roa_tara_redirects = Set("#REDIRECT")
    private def ru_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def rue_redirects = Set("#ПЕРЕНАПРАВЛЕННЯ","#ПЕРЕНАПР","#перенапр","#перенаправление","#REDIRECT")
    private def rw_redirects = Set("#REDIRECT")
    private def sa_redirects = Set("#पुनर्निदेशन","#REDIRECT")
    private def sah_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def sc_redirects = Set("#REDIRECT")
    private def scn_redirects = Set("#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def sco_redirects = Set("#REDIRECT")
    private def sd_redirects = Set("#چوريو","#REDIRECT")
    private def se_redirects = Set("#OĐĐASITSTIVREN","#STIVREN","#REDIRECT")
    private def sg_redirects = Set("#REDIRECTION","#REDIRECT")
    private def sh_redirects = Set("#PREUSMJERI","#PREUSMERI","#REDIRECT")
    private def si_redirects = Set("#යළියොමුව","#REDIRECT")
    private def simple_redirects = Set("#REDIRECT")
    private def sk_redirects = Set("#presmeruj","#REDIRECT")
    private def sl_redirects = Set("#PREUSMERITEV","#REDIRECT")
    private def sm_redirects = Set("#REDIRECT")
    private def sn_redirects = Set("#REDIRECT")
    private def so_redirects = Set("#REDIRECT")
    private def sq_redirects = Set("#RIDREJTO","#REDIRECT")
    private def sr_redirects = Set("#Преусмери","#преусмери","#ПРЕУСМЕРИ","#Преусмјери","#преусмјери","#ПРЕУСМЈЕРИ","#redirect","#REDIRECT")
    private def srn_redirects = Set("#STIR","#DOORVERWIJZING","#REDIRECT")
    private def ss_redirects = Set("#REDIRECT")
    private def st_redirects = Set("#REDIRECT")
    private def stq_redirects = Set("#WEITERLEITUNG","#REDIRECT")
    private def su_redirects = Set("#ALIH","#REDIRECT")
    private def sv_redirects = Set("#OMDIRIGERING","#REDIRECT")
    private def sw_redirects = Set("#REDIRECT")
    private def szl_redirects = Set("#PATRZ","#PRZEKIERUJ","#TAM","#REDIRECT")
    private def ta_redirects = Set("#வழிமாற்று","#REDIRECT")
    private def te_redirects = Set("#దారిమార్పు","#REDIRECT")
    private def tet_redirects = Set("#REDIRECT")
    private def tg_redirects = Set("#REDIRECT")
    private def th_redirects = Set("#เปลี่ยนทาง","#REDIRECT")
    private def ti_redirects = Set("#REDIRECT")
    private def tk_redirects = Set("#REDIRECT")
    private def tl_redirects = Set("#REDIRECT")
    private def tn_redirects = Set("#REDIRECT")
    private def to_redirects = Set("#REDIRECT")
    private def tpi_redirects = Set("#REDIRECT")
    private def tr_redirects = Set("#YÖNLENDİRME","#YÖNLENDİR","#REDIRECT")
    private def ts_redirects = Set("#REDIRECT")
    private def tt_redirects = Set("#ЮНӘЛТҮ","#перенаправление","#перенапр","#REDIRECT")
    private def tum_redirects = Set("#REDIRECT")
    private def tw_redirects = Set("#REDIRECT")
    private def ty_redirects = Set("#REDIRECTION","#REDIRECT")
    private def udm_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def ug_redirects = Set("#REDIRECT")
    private def uk_redirects = Set("#ПЕРЕНАПРАВЛЕННЯ","#ПЕРЕНАПР","#перенапр","#перенаправление","#REDIRECT")
    private def ur_redirects = Set("#REDIRECT")
    private def uz_redirects = Set("#REDIRECT")
    private def ve_redirects = Set("#REDIRECT")
    private def vec_redirects = Set("#VARDA","#RINVIA","#RINVIO","#RIMANDO","#REDIRECT")
    private def vep_redirects = Set("#suuna","#REDIRECT")
    private def vi_redirects = Set("#đổi","#REDIRECT")
    private def vls_redirects = Set("#DOORVERWIJZING","#REDIRECT")
    private def vo_redirects = Set("#REDIRECT")
    private def wa_redirects = Set("#REDIRECTION","#REDIRECT")
    private def war_redirects = Set("#REDIRECT")
    private def wo_redirects = Set("#REDIRECTION","#REDIRECT")
    private def wuu_redirects = Set("#重定向","#REDIRECT")
    private def xal_redirects = Set("#перенаправление","#перенапр","#REDIRECT")
    private def xh_redirects = Set("#REDIRECT")
    private def xmf_redirects = Set("#გადამისამართება","#REDIRECT")
    private def yi_redirects = Set("#ווייטערפירן","#הפניה","#REDIRECT")
    private def yo_redirects = Set("#REDIRECT")
    private def za_redirects = Set("#重定向","#REDIRECT")
    private def zea_redirects = Set("#DOORVERWIJZING","#REDIRECT")
    private def zh_redirects = Set("#重定向","#REDIRECT")
    private def zh_classical_redirects = Set("#REDIRECT")
    private def zh_min_nan_redirects = Set("#REDIRECT")
    private def zh_yue_redirects = Set("#REDIRECT")
    private def zu_redirects = Set("#REDIRECT")



    def apply(language : Language) = map(language.wikiCode)
}
