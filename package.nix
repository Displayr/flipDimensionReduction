{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "flipDimensionReduction";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = ''
    Tools for dimension reduction (e.g. principal components analysis,
    correspondence analysis) using the flip Project conventions.
  '';
  propagatedBuildInputs = with pkgs.rPackages; [ 
    verbs
    MASS
    flipTables
    flipTransformations
    GPArotation
    rhtmlCombinedScatter
    flipData
    flipChartBasics
    weights
    rhtmlLabeledScatter
    class
    flipU
    psych
    nloptr
    flipImputation
    Rtsne
    flipFormat
    rhtmlHeatmap
    ca
    rhtmlMoonPlot
    flipStatistics
    plotly
    flipStandardCharts
  ];
}
