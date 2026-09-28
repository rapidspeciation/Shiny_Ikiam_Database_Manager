// The workbook's own checks, copied from the Google Sheet (conditional formats
// and data validation, read on 2026-09-28). Google applies them when someone
// types in the sheet, but not to writes through the API, so the app applies
// them itself: repeated IDs are refused and coloured, values outside a strict
// list are refused, and every value outside its list gets the red corner.
// Shared by the server (server/batch.mjs) and the grids (frontend).

/** Columns whose values must not repeat within the sheet ("NA" and blanks excepted). */
export const UNIQUE = {
  Collection_data: ['Insectary_ID', 'CAM_ID', 'CAM_ID_insectary'],
  Insectary_data: ['Insectary_ID', 'CAM_ID', 'CAM_ID_CollData'],
  'F1/F2_MutationRate': ['Insectary_ID', 'CAM_ID'],
  CRISPR: ['CAM_ID'],
  Pheromones_data: ['CAM_ID'],
  Wing_tissue: ['CAM_ID'],
  Life_History: ['CAM_ID'],
  Barcoding_DNA: ['CAM_ID'],
  Sperm_dissections: ['Father_CAMid', 'Mother_CAMid'],
};

/** Tube barcodes are unique across the whole workbook. */
export const TUBE_FIELD = /^Tube_\d_id(?:_LEGS)?$/;

/**
 * Dropdown lists: fixed `values`, or `from` another sheet's column. `strict`
 * lists reject other values in Google Sheets, and so does the app for new values.
 */
export const LISTS = {
  "Collection_data": {
    "Release_Collect": {
      "values": [
        "Collected_Preserved",
        "Collected_Sent2Insectary",
        "Mark_Released",
        "Released_Unmarked",
        "NA"
      ],
      "strict": true
    },
    "CAM_ID_insectary": {
      "from": [
        "Lists",
        "InsectaryWild&Reared_CAMid"
      ],
      "strict": true
    },
    "CAM_ID": {
      "from": [
        "Lists",
        "Wild_indv_CAMid"
      ],
      "strict": true
    },
    "Tube_1_tissue": {
      "from": [
        "Lists",
        "ORGANISM_PART"
      ],
      "strict": true
    },
    "Tube_2_tissue": {
      "from": [
        "Lists",
        "ORGANISM_PART"
      ],
      "strict": true
    },
    "Tube_3_tissue": {
      "from": [
        "Lists",
        "ORGANISM_PART"
      ],
      "strict": true
    },
    "Purpose": {
      "from": [
        "Lists",
        "Research_purpose"
      ],
      "strict": true
    },
    "SPECIES": {
      "from": [
        "Taxonomy_v18Jun25",
        "species"
      ],
      "strict": true
    },
    "Identifier": {
      "from": [
        "Lists",
        "Abbr_name"
      ]
    },
    "ID_status": {
      "values": [
        "COMPLETE",
        "Complete_but_verify",
        "Incomplete_family_only",
        "Incomplete_tribe_only",
        "Incomplete_genus_only",
        "To_identify",
        "NA"
      ],
      "strict": true
    },
    "Sex": {
      "values": [
        "female",
        "male",
        "male ?",
        "female ?",
        "NOT_COLLECTED"
      ],
      "strict": true
    },
    "Collection_location": {
      "from": [
        "Location_data",
        "Collection_location"
      ],
      "strict": true
    },
    "Transect_section": {
      "values": [
        "1",
        "2",
        "3",
        "4",
        "NA"
      ],
      "strict": true
    },
    "Bait": {
      "values": [
        "Banana",
        "Fish",
        "NA"
      ],
      "strict": true
    },
    "Forest_stratum": {
      "values": [
        "Understorey",
        "Canopy",
        "NA"
      ],
      "strict": true
    },
    "Collector": {
      "from": [
        "Lists",
        "Abbr_name"
      ],
      "strict": true
    },
    "Rainfall": {
      "values": [
        "SR_(strong_rain)",
        "WR_(weak_rain)",
        "DZ_(drizzle)",
        "DY_(dry)",
        "NA"
      ],
      "strict": true
    },
    "Cloud_cover": {
      "values": [
        "CD_(cloudy_dark)",
        "CL_(cloudy_light)",
        "S&C_(sun_&_cloud_patches)",
        "S_(cloudless_sunny)",
        "NA"
      ],
      "strict": true
    },
    "Preservation_medium": {
      "values": [
        "Flash frozen",
        "DMSO buffer",
        "DMSO buffer & Flash frozen",
        "Ethanol",
        "Ethanol & Flash frozen",
        "RNAlater",
        "AllProtect & RNAlater",
        "Dry",
        "Methanol",
        "NOT_PRESERVED",
        "NOT_COLLECTED"
      ],
      "strict": true
    },
    "Preserved_dead_alive": {
      "values": [
        "Dead",
        "Alive",
        "NOT_PRESERVED",
        "NA"
      ],
      "strict": true
    },
    "Splitted_body": {
      "values": [
        "Yes",
        "No",
        "NA"
      ],
      "strict": true
    },
    "Location_WholeBody": {
      "from": [
        "Lists",
        "Tissue locations"
      ],
      "strict": true
    },
    "Location_Head": {
      "from": [
        "Lists",
        "Tissue locations"
      ],
      "strict": true
    },
    "Location_Torax": {
      "from": [
        "Lists",
        "Tissue locations"
      ],
      "strict": true
    },
    "Location_abdomen": {
      "from": [
        "Lists",
        "Tissue locations"
      ],
      "strict": true
    },
    "Location_Legs": {
      "from": [
        "Lists",
        "Tissue locations"
      ],
      "strict": true
    },
    "Location_wings": {
      "from": [
        "Lists",
        "Tissue locations"
      ],
      "strict": true
    }
  },
  "Insectary_data": {
    "Wild_Reared": {
      "values": [
        "Wild-caught",
        "Reared",
        "NA"
      ],
      "strict": true
    },
    "CLUTCH NUMBER": {
      "from": [
        "Insectary_stocks",
        "CLUTCH NUMBER"
      ],
      "strict": true
    },
    "Stock_of_origin": {
      "values": [
        "deceptus",
        "messenoides",
        "intermedia",
        "NA"
      ]
    },
    "SPECIES": {
      "from": [
        "Lists",
        "Insectary_species"
      ]
    },
    "Sex": {
      "values": [
        "female",
        "male",
        "NOT_COLLECTED",
        "NA"
      ],
      "strict": true
    },
    "Collection_location": {
      "from": [
        "Location_data",
        "Collection_location"
      ],
      "strict": true
    },
    "Death_cause": {
      "values": [
        "Disappearance",
        "Deformed",
        "Spider",
        "Ants",
        "Eaten",
        "Heat stroke",
        "Unknown",
        "Unknown - Only wings",
        "Killed_Preserved",
        "Other",
        "NA"
      ]
    },
    "Research_purpose": {
      "from": [
        "Lists",
        "Research_purpose"
      ],
      "strict": true
    },
    "Pedigree": {
      "values": [
        "Yes",
        "No",
        "NA"
      ]
    },
    "LIFESTAGE": {
      "values": [
        "Egg",
        "1st instar larva",
        "2nd instar larva",
        "3rd instar larva",
        "4th instar larva",
        "5th instar larva",
        "Pre-pupa",
        "Pupa day 1",
        "Pupa day 2",
        "Pupa day 3",
        "Pupa day 4",
        "Pupa day 5",
        "Pupa day 6",
        "Pupa day 7",
        "Pupa day 8",
        "Pupa day 9",
        "Pupa day 10",
        "Pupa day 11",
        "Pupa day 12",
        "Adult",
        "NOT_COLLECTED",
        "NA"
      ],
      "strict": true
    },
    "CAM_ID": {
      "from": [
        "Lists",
        "InsectaryWild&Reared_CAMid"
      ],
      "strict": true
    },
    "Tube_1_tissue": {
      "from": [
        "Lists",
        "ORGANISM_PART"
      ],
      "strict": true
    },
    "T1_Preservation_medium": {
      "values": [
        "Flash frozen",
        "DMSO",
        "DMSO_FlashFrozen",
        "Ethanol",
        "RNAlater",
        "Dry",
        "NOT_COLLECTED"
      ],
      "strict": true
    },
    "Tube_2_tissue": {
      "from": [
        "Lists",
        "ORGANISM_PART"
      ],
      "strict": true
    },
    "T2_Preservation_medium": {
      "values": [
        "Flash frozen",
        "DMSO",
        "DMSO_FlashFrozen",
        "Ethanol",
        "RNAlater",
        "Dry",
        "NOT_COLLECTED"
      ],
      "strict": true
    },
    "Tube_3_tissue": {
      "from": [
        "Lists",
        "ORGANISM_PART"
      ],
      "strict": true
    },
    "Tube_4_tissue": {
      "from": [
        "Lists",
        "ORGANISM_PART"
      ],
      "strict": true
    },
    "Preservation_medium": {
      "values": [
        "Flash frozen",
        "DMSO",
        "DMSO_FlashFrozen",
        "Ethanol",
        "RNAlater",
        "Dry",
        "NOT_COLLECTED"
      ],
      "strict": true
    },
    "Preserved_Dead_Alive": {
      "values": [
        "Dead",
        "Alive",
        "NA"
      ],
      "strict": true
    },
    "Location_body": {
      "from": [
        "Lists",
        "Tissue locations"
      ],
      "strict": true
    }
  },
  "Insectary_stocks": {
    "Generation": {
      "values": [
        "NA",
        "F1",
        "F2",
        "Backcross"
      ]
    },
    "SPECIES": {
      "from": [
        "Lists",
        "Insectary_species"
      ]
    },
    "INSECTARY OR LABORATORY": {
      "values": [
        "Insectary",
        "Laboratory"
      ]
    }
  },
  "F1/F2_MutationRate": {
    "Generation": {
      "values": [
        "Generation",
        "P",
        "F1",
        "F2",
        "Backcross",
        "NA"
      ],
      "strict": true
    },
    "Mating": {
      "values": [
        "Yes",
        "No"
      ],
      "strict": true
    },
    "Split_tube": {
      "values": [
        "Yes",
        "No"
      ],
      "strict": true
    }
  },
  "SamplingDay_data": {
    "Location": {
      "from": [
        "Location_data",
        "Collection_location"
      ],
      "strict": true
    },
    "Purpose": {
      "values": [
        "Monitoring",
        "Abundance estimation"
      ],
      "strict": true
    }
  },
  "CRISPR": {
    "Guide": {
      "values": [
        "2",
        "3",
        "2-3",
        "2B",
        "2A",
        "2C",
        "2D",
        "2A-2D",
        "2-3A",
        "2-3B",
        "2C-2D",
        "2A-2C",
        "2B-2D",
        "2B-2C",
        "3A-2B",
        "No guide",
        "2A-2B-2C-2D",
        "2A-2B",
        "3A",
        "Iv-AB",
        "Peak-AB"
      ],
      "strict": true
    },
    "Stock_of_origin": {
      "from": [
        "Lists",
        "Insectary_species"
      ]
    },
    "Species": {
      "from": [
        "Lists",
        "Insectary_species"
      ]
    },
    "Sex": {
      "values": [
        "female",
        "male",
        "NA"
      ],
      "strict": true
    },
    "Mutant": {
      "values": [
        "Yes",
        "No",
        "NA",
        "Check"
      ],
      "strict": true
    },
    "CAM_ID": {
      "from": [
        "Lists",
        "CRISPRonly_CAMid"
      ],
      "strict": true
    }
  }
};

export const isUnique = (sheet, field) => !!UNIQUE[sheet]?.includes(field) || TUBE_FIELD.test(field);
export const blankOrNA = value =>
  value === null || value === undefined || /^\s*(|NA|N\/A)\s*$/i.test(String(value));
/** IDs always hold a digit (CAM079895, FS90415305, N5D, 85Y); texts such as "not given" or NOT_COLLECTED may repeat. */
export const isIdValue = value => !blankOrNA(value) && /\d/.test(String(value));
