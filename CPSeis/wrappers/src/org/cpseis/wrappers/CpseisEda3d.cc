//////// DO NOT EDIT THIS FILE - it is machine generated ////////

#include "CpseisEda3d.hh"

//------------------ fortran spelling adjustments --------------------//
//------------------ fortran spelling adjustments --------------------//
//------------------ fortran spelling adjustments --------------------//

#if NEED_UNDERSCORE
#define eda3d_wrap_create             eda3d_wrap_create_
#define eda3d_wrap_delete             eda3d_wrap_delete_
#define eda3d_wrap_update             eda3d_wrap_update_
#define eda3d_wrap_wrapup             eda3d_wrap_wrapup_
#define eda3d_wrap_oneset             eda3d_wrap_oneset_
#define eda3d_wrap_twosets            eda3d_wrap_twosets_
#elif NEED_CAPITALS
#define eda3d_wrap_create             EDA3D_WRAP_CREATE
#define eda3d_wrap_delete             EDA3D_WRAP_DELETE
#define eda3d_wrap_update             EDA3D_WRAP_UPDATE
#define eda3d_wrap_wrapup             EDA3D_WRAP_WRAPUP
#define eda3d_wrap_oneset             EDA3D_WRAP_ONESET
#define eda3d_wrap_twosets            EDA3D_WRAP_TWOSETS
#endif

//----------------------- fortran prototypes -------------------------//
//----------------------- fortran prototypes -------------------------//
//----------------------- fortran prototypes -------------------------//

extern "C"
    {
    CpseisBase::ModuleCreate   eda3d_wrap_create;
    CpseisBase::ModuleDestroy  eda3d_wrap_delete;
    CpseisBase::ModuleUpdate   eda3d_wrap_update;
    CpseisBase::ModuleWrapup   eda3d_wrap_wrapup;
    CpseisBase::ModuleOneset   eda3d_wrap_oneset;
    CpseisBase::ModuleTwosets  eda3d_wrap_twosets;
    }

//------------------------ constructor -------------------------------//
//------------------------ constructor -------------------------------//
//------------------------ constructor -------------------------------//

CpseisEda3d::CpseisEda3d() : CpseisBase ("EDA3D",
                                       eda3d_wrap_create,
                                       eda3d_wrap_delete,
                                       eda3d_wrap_update,
                                       eda3d_wrap_wrapup,
                                       eda3d_wrap_oneset,
                                       eda3d_wrap_twosets) {}

//------------------------------ end ---------------------------------//
//------------------------------ end ---------------------------------//
//------------------------------ end ---------------------------------//