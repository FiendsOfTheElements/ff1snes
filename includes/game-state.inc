.include "banks.inc"

ship_pos     = $0000
airship_pos  = $0002
player_pos   = $0004
curr_map     = $0006
curr_vehicle = $0007
gold         = $0008
; any other miscellaneous can go here

quest_items  = $0060 ; one byte per item
inventory    = $0080 ; two bytes per item: quantity and item id

game_flags   = $0100 ; one byte stores event flags, treasure flags, and NPC visibility as bits

party        = $0200 ; start of party data
ch_size      = $0080 ; each party character gets 128 bytes
ch_name      = $0000 ; 8 bytes
ch_class     = $0008
ch_status    = $0009
ch_str       = $000A
ch_agi       = $000B
ch_int       = $000C
ch_vit       = $000D
ch_luck      = $000E
ch_mag_def   = $000F
ch_level     = $0010
ch_exp       = $0011 ; 4 bytes
ch_curr_hp   = $0015 ; 2 bytes
ch_max_hp    = $0017 ; 2 bytes
ch_eq_weapon = $0019
ch_eq_shield = $0020
ch_eq_helm   = $0021
ch_eq_armor  = $0022
ch_eq_arms   = $0023
; 30 free bytes for character stats, we can put counters here?
ch_spells    = $0040 ; 4 bytes per level, up to 4 spells
ch_curr_mp   = $0060 ; 1 byte per level
ch_max_mp    = $0068 ; 1 byte per level
; 16 free bytes at the end of each character data

; Except we use 6 of them at the very end for validation
game_state_size = $0400 ; each save slot will be this long
game_check_AAAA = $03FE ; if both of these are 0, there is no game saved here
game_check_5555 = $03FC ; if either of these are not 0 and not $AAAA/$5555, the game is corrupt
game_check_sum  = $03FA ; if this is wrong, the game is corrupt

ship_pos_far = ship_pos | (BANK_SRAM << 16)
airship_pos_far = airship_pos | (BANK_SRAM << 16)
player_pos_far = player_pos | (BANK_SRAM << 16)
curr_map_far = curr_map | (BANK_SRAM << 16)
curr_vehicle_far = curr_vehicle | (BANK_SRAM << 16)
gold_far = gold | (BANK_SRAM << 16)

quest_items_far = quest_items | (BANK_SRAM << 16)
inventory_far = inventory | (BANK_SRAM << 16)

party_far = party | (BANK_SRAM << 16)
