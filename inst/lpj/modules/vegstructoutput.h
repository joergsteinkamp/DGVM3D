///////////////////////////////////////////////////////////////////////////////////////
/// \file vegstructoutput.h
/// \brief Output module for patch based vegetation structure
///
/// \author Joerg Steinkamp
/// $Date: Thu Nov  3 11:15:37 CET 2016 $
///
///////////////////////////////////////////////////////////////////////////////////////
#ifndef LPJ_GUESS_VEGSTRUCT_OUTPUT_H
#define LPJ_GUESS_VEGSTRUCT_OUTPUT_H
#include "outputmodule.h"
#include "outputchannel.h"
#include "gutil.h"
namespace GuessOutput {
  class VegstructOutput : public OutputModule {
  public:
    VegstructOutput();
    ~VegstructOutput();
    // implemented functions inherited from OutputModule
    // (see documentation in OutputModule)
    void init();
    void outannual(Gridcell& gridcell);
    void outdaily(Gridcell& gridcell);
  private:
    xtring file_vegstruct;
    FILE *out_vegstruct;
  };
}
#endif
