
# paleo_perc_change v1.0.1

- changed the colors of the bar plots returned by the change_perc_relto() 
  function. Now uses colors from the diverging & Colour-vision-deficiency (CVD)
  friendly color palette 'vik' (Crameri, F. (2018), Scientific colour maps, 
  Zenodo, doi:10.5281/zenodo.1243862).


# paleo_perc_change v1.0.0

- changed names of arguments (now they refer to 'reference period' and to the
  'reference value' rather than to 'baselines');
- added documentation for arguments that were already included in the
  change_perc_stepwise() function;
- fixed a bug in the calculation of the rate of mean percentage change per 
  unit-time (change_perc_baseln() function);
- added an argument to the change_perc_baseln() function. Specifically, the 
  'reference_level_desc' allows adding additional information on the reference
  level (e.g. units, source) to the subtitle of the plot;
- added a legend to the plot returned by the change_perc_refp() function;
- added text to document the functions and the scripts;
- changed name of the change_perc_baseln() function -> change_perc_relto().


# paleo_perc_change v0.1.0

- Initial release of the scripts and function
