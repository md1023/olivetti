# battletech_roster_skills.py

from typing import Dict, List, Tuple, Union, Any

# Define types
BVEntry = Dict[str, int]
MechDB = Dict[str, BVEntry]
RosterEntry = Dict[str, Union[str, int, float]]

mech_db: MechDB = {
    "Marauder": {"BV2": 1700},
    "Rifleman": {"BV2": 1150},
    "Griffin": {"BV2": 1200},
    "Catapult": {"BV2": 1450},
    "Bulldog": {"BV2": 600},
    "Scorpion": {"BV2": 400},
    "LRM Tracked Launcher": {"BV2": 450}
}

def get_skill_modifier(gunnery: int, piloting_or_driving: int) -> float:
    base = 1.0
    modifier = base + (4 - gunnery) * 0.1 + (5 - piloting_or_driving) * 0.05
    return round(modifier, 2)

def is_vehicle(unit_name: str) -> bool:
    return unit_name.lower() in ["bulldog", "scorpion", "lrm tracked launcher"]

def show_unit_list() -> None:
    print("\nAvailable Units:")
    for idx, (name, stats) in enumerate(mech_db.items(), start=1):
        unit_type = "Vehicle" if is_vehicle(name) else "Mech"
        print(f"{idx}. {name} ({unit_type}, BV2: {stats['BV2']})")

def create_roster() -> Tuple[List[RosterEntry], int]:
    roster: List[RosterEntry] = []
    total_bv2 = 0
    unit_list = list(mech_db.items())

    while True:
        show_unit_list()
        choices = input("\nEnter unit number(s) (e.g. 1 2 3 or 1,2) or press Enter to finish: ").strip()
        if choices == "":
            break

        raw_indices = [part for part in choices.replace(",", " ").split()]
        invalid = [c for c in raw_indices if not c.isdigit() or not (1 <= int(c) <= len(unit_list))]

        if invalid:
            print(f"Invalid selection(s): {' '.join(invalid)}. Try again.")
            continue

        for c in raw_indices:
            idx = int(c) - 1
            unit_name, stats = unit_list[idx]

            try:
                gunnery_input = input(f"  [{unit_name}] Enter Gunnery skill [default 4]: ").strip()
                gunnery = int(gunnery_input) if gunnery_input else 4

                if is_vehicle(unit_name):
                    driving_input = input(f"  [{unit_name}] Enter Driving skill [default 5]: ").strip()
                    driving = int(driving_input) if driving_input else 5
                    skill = driving
                    skill_label = "Driving"
                else:
                    piloting_input = input(f"  [{unit_name}] Enter Piloting skill [default 5]: ").strip()
                    piloting = int(piloting_input) if piloting_input else 5
                    skill = piloting
                    skill_label = "Piloting"
            except ValueError:
                print("  Invalid skill input. Skipping this unit.")
                continue

            base_bv = stats["BV2"]
            modifier = get_skill_modifier(gunnery, skill)
            adjusted_bv = round(base_bv * modifier)

            roster.append({
                "name": unit_name,
                "gunnery": gunnery,
                skill_label.lower(): skill,
                "base_bv": base_bv,
                "modifier": modifier,
                "adjusted_bv": adjusted_bv,
                "skill_label": skill_label
            })

            total_bv2 += adjusted_bv
            print(f"  Added {unit_name} → Adjusted BV2: {adjusted_bv} (modifier: {modifier})")
            print(f"  → Current Total Adjusted BV2: {total_bv2}")

    return roster, total_bv2

def main() -> None:
    print("=== BattleTech Roster Builder (with Pilot Skill Modifiers) ===")
    roster, total_bv2 = create_roster()

    print("\n=== Final Roster ===")
    for mech in roster:
        skill_type = mech["skill_label"]
        print(
            f"- {mech['name']} (BV2: {mech['base_bv']}, Gunnery: {mech['gunnery']}, {skill_type}: {mech[skill_type.lower()]}, Modifier: {mech['modifier']}, Adjusted BV2: {mech['adjusted_bv']})"
        )

    print(f"\nTotal Adjusted BV2: {total_bv2}")

if __name__ == "__main__":
    main()