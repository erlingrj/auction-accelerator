# Lessons learned

## Design-phase
- Spend much much more time in design. Re-writing modules takes forever

## Configs
- Configuration turned out to be not trivial. Alot of IO bundles are used by different modules which have their own configs

### Solution 1:
- Have a virtual base class config for all configs with the most important stuff (bit-widths mem interfaces and so on).
- Have shared IO use those configs

### Solution 2: Like BOOM, use implict parameters and just one gigantic config file
- Downside: Yaman made a nice design space exploration based on configs.