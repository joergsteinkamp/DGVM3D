///////////////////////////////////////////////////////////////////////////////////////
/// \file vegstructoutput.cpp
/// \brief Output module for patch based vegetation structure
///
/// \author Joerg Steinkamp
/// $Date: Thu Nov  3 11:15:37 CET 2016 $
///
///////////////////////////////////////////////////////////////////////////////////////
#include "config.h"
#include "vegstructoutput.h"
#include "parameters.h"
#include "guess.h"
namespace GuessOutput {
    REGISTER_OUTPUT_MODULE("vegstruct", VegstructOutput)
    VegstructOutput::VegstructOutput() {
    declare_parameter("file_vegstruct", &file_vegstruct, 300, "Detailed vegetation structure");
  }
  VegstructOutput::~VegstructOutput() {
  }
  void VegstructOutput::init() {
    if (file_vegstruct != "") {
      std::string full_path = (char*) file_vegstruct;
      out_vegstruct = fopen(full_path.c_str(), "w");
      if (out_vegstruct == NULL) {
        fail("Could not open %s for output\n"                         \
             "Close the file if it is open in another application",
             full_path.c_str());
      } else {
        dprintf("dummy\n");
        fprintf(out_vegstruct, "Lon Lat Year SID PID VID Pft Lifeform LeafType PhenType Pathway Age LAI ShadeType N DBH Height Crownarea\n");
      }
    }
  }
  void VegstructOutput::outdaily(Gridcell& gridcell) {
    return;
  }
  void VegstructOutput::outannual(Gridcell& gridcell) {
    if (file_vegstruct == "")
      return;
    if (date.year >= nyear_spinup-50) {
      double lon = gridcell.get_lon();
      double lat = gridcell.get_lat();
      Gridcell::iterator gc_itr = gridcell.begin();
      while (gc_itr != gridcell.end()) {
        Stand& stand = *gc_itr;
        stand.firstobj();
        while (stand.isobj) {
          Patch& patch = stand.getobj();
          Vegetation& vegetation = patch.vegetation;
          vegetation.firstobj();
          while (vegetation.isobj) {
            Individual& indiv=vegetation.getobj();
            // guess2008 - alive check added
            if (indiv.id != -1 && indiv.alive) {
              fprintf(out_vegstruct, "%7.2f %6.2f %4i ", lon, lat, date.get_calendar_year() );
              fprintf(out_vegstruct, " %i ",    stand.id);
              fprintf(out_vegstruct, " %i ",    patch.id);
              fprintf(out_vegstruct, " %i ",    indiv.id);
              fprintf(out_vegstruct, " %10s ",  (char*) indiv.pft.name);
              fprintf(out_vegstruct, " %i ",    indiv.pft.lifeform);
              fprintf(out_vegstruct, " %i ",    indiv.pft.leafphysiognomy);
              fprintf(out_vegstruct, " %i ",    indiv.pft.phenology);
              fprintf(out_vegstruct, " %i ",    indiv.pft.pathway);
              fprintf(out_vegstruct, " %4.0f ", indiv.age);
              fprintf(out_vegstruct, " %6.2f ", indiv.lai);
              if (indiv.pft.lifeform == TREE) {
                fprintf(out_vegstruct, " %4.1f ", indiv.pft.alphar);
                fprintf(out_vegstruct, " %4.0f ", indiv.densindiv * patcharea);
                fprintf(out_vegstruct, " %7.2f ", pow(indiv.height/indiv.pft.k_allom2,1.0/indiv.pft.k_allom3));
                fprintf(out_vegstruct, " %8.2f ", indiv.height);
                fprintf(out_vegstruct, " %8.2f ", indiv.crownarea);
              } else if (indiv.pft.lifeform == GRASS) {
                fprintf(out_vegstruct, " %4.1f ", -1.0);
                fprintf(out_vegstruct, " %i ",     1);
                fprintf(out_vegstruct, " %i ",    -1);
                fprintf(out_vegstruct, " %i ",    -1);
                fprintf(out_vegstruct, " %i ",    -1);
              }
              fprintf(out_vegstruct, "\n");
            }
            vegetation.nextobj();
          }
          stand.nextobj();
        }
        ++gc_itr;
      }
    }
  } // END of void VegStructOutput::outannual
} // END of namespace VegStructOutput
