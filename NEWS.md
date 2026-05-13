# CAWET (development version)

# CAWET 1.1.0 (2026-03-30)

## Bug fixes

- Correction or suppression of the `daily_ETc_sum` graph:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#34](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/34)
- Wrong calculation of mean irrigation in mm by grid cell:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#45](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/45)
- RPG does not correspond to RPG retrieved from RADIS:
  [Claire Richert](https://forge.inrae.fr/claire.richert),
  [#49](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/49)
- Wrong area kept in Script 1 after aggregation at plot/network/crop level:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#51](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/51)
- Crash when using Info et Sol AWC on dev branch:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#54](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/54)
- Hazardous geographical join of depth and AWC:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#55](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/55)
- Crop codes from ODR source are not recognized by CAWET for simulation:
  [Claire Richert](https://forge.inrae.fr/claire.richert),
  [#61](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/61)
- Plot maps: missing years before the last year:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#63](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/63)

## Enhancements

- Modification of the `install` file name for a better identification in the
  downloaded directory:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#25](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/25)
- Make download of soil texture optional:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#40](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/40)
- Auto-update CAWET package to the latest version at startup:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#42](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/42)
- Do not produce `warning_missing_crops.csv` when empty:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#43](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/43)
- Remove verbose "Joining with `by = join_by(...)`" messages from the console
  :
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#44](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/44)
- Improve format of console outputs:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#48](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/48)
- Remove dependency to online file `coordonnees-des-mailles_339.csv`:
  [#53](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/53)

## Documentation

- Update website and documentation:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#17](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/17)
- Development badges are displayed on the release version of the pkgdown
  website:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#57](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/57)
- Improve documentation for v1.1.0:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#59](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/59)

## Internal changes

- Fix package imports (remove `library()` usage in functions and clean imports):
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#8](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/8)

# CAWET 1.0.2 (2026-01-30)

- Correction of incorrect geometries in the RPG causing junction problems with
other sf files
  [Pierre Rouault](https://forge.inrae.fr/pierre.rouault)
  [#37](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/37)
- Choose a better parametrisation for VRG to not have a root developpement
 during the season
  [Pierre Rouault](https://forge.inrae.fr/pierre.rouault)
  [#35](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/35)
- Correct graph in Script2
  [Pierre Rouault](https://forge.inrae.fr/pierre.rouault)
  [#9](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/9)
- Correct saving Formodification soil data
  [Pierre Rouault](https://forge.inrae.fr/pierre.rouault)
  [#2](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/2)

# CAWET 1.0.1 (2026-01-29)

## Bug fixes

- Correction of the non generation of agregate table when the last crop is not
  parametred
  [Pierre Rouault](https://forge.inrae.fr/pierre.rouault)
  [#36](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/36)

# CAWET 1.0.0 (2025-12-19)

## New features

- Initial development of the CAWET R scripts:
  [Pierre Rouault](https://forge.inrae.fr/pierre.rouault)

## Bug fixes

- Crash when using ordonnanceur automatically built:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#21](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/21)
- CAWET installation: remove Rtools dependency:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#19](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/19)

## Documentation

- Generate documentation website with pkgdown:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#5](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/5)

## Enhancements

- Define default Working path from ordonnanceur location:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#24](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/24)
- Installation script for Windows users:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#14](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/14)

## Internal changes

- Refactor folder organization for compliance with R package structure:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#6](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/6)
- Add CI automatic test on the provided example:
  [David Dorchies](https://forge.inrae.fr/david.dorchies),
  [#18](https://forge.inrae.fr/umr-g-eau/cawet/-/issues/18)
