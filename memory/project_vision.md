---
name: Full project vision and spin-off goals
description: Complete vision for the MLB scouting pipeline and planned spin-off analytics projects
type: project
---

The project is a fully automated MLB scouting and game-context analytics system that generates structured, data-driven scouting reports for MLB teams and individual matchups. Built in R as a modular data pipeline.

**Core architecture:**
- Canonical ID spine (player_master_ids) resolves MLBAM, FanGraphs, BBRef, and Lahman IDs
- Phased pipeline: 00_setup → 01_ids → 02_static_context → 03_rosters → 04_game_context → 05_performance → 06_derived → 07_validation
- Grain: player-season-team for performance; game-level for context
- Sources: MLB Stats API, Statcast, FanGraphs, Lahman (+ Baseball Reference in progress)

**Performance layers needed:**
- Offense ✓, Pitching ✓, Defense (nearly done), Baserunning (not yet built)
- Statcast quality-of-contact metrics
- Park factors (coordinates exist, run-factor calculations not yet built)

**Derived layer (Phase 06) — critical for spin-offs:**
- Rolling window stats (last 7/14/30 days)
- Platoon splits (vs LHP/RHP)
- Home/away splits
- Percentile rankings per stat

**Stat dictionary:**
- Definitions for all stats across all sources
- Percentile ranges so stats can be labeled in plain English
- Powers both report narrative and projection explainability

**End output:**
- Daily-runnable scouting report engine
- HTML/PDF reports for specific matchups (e.g. Braves vs X on date Y)
- Covers: offensive profile, pitching tendencies, bullpen structure, defensive strengths/weaknesses, baserunning, park effects, game-day conditions

**Planned spin-off projects:**
1. Player projections (needs historical rolling data)
2. Fantasy baseball analytics
3. Daily hit probability model (who has best chance to get a hit today)
4. Stat dictionary with definitions and ranges for all sources

**Why:** Build a repeatable system that transforms raw baseball data into actionable scouting intelligence without manually assembling data from multiple sources each day.
