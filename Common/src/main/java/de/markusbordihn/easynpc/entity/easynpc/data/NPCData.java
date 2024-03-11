package de.markusbordihn.easynpc.entity.easynpc.data;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.LivingEntity;

public interface NPCData<T extends LivingEntity> extends EasyNPC<T> {

  String DATA_EASY_NPC_DATA_VERSION_TAG = "EasyNPCVersion";

  default void addAdditionalNPCData(CompoundTag compoundTag) {
    compoundTag.putInt(DATA_EASY_NPC_DATA_VERSION_TAG, Constants.NPC_DATA_VERSION);
  }

  default void readAdditionalNPCData(CompoundTag compoundTag) {

    // Read Easy NPC Data Version to check for compatibility issues.
    if (compoundTag.contains(DATA_EASY_NPC_DATA_VERSION_TAG)) {
      int npcDataVersion = compoundTag.getInt(DATA_EASY_NPC_DATA_VERSION_TAG);
      if (npcDataVersion > Constants.NPC_DATA_VERSION) {
        log.warn(
            "Incompatible Easy NPC Data with version {} > {} for {}!",
            npcDataVersion,
            Constants.NPC_DATA_VERSION,
            this);
        log.warn("Will try to load data, but expect issues!");
      } else if (npcDataVersion < Constants.NPC_DATA_VERSION) {
        log.warn("Outdated Easy NPC Data with version {} for {}!", npcDataVersion, this);
        log.warn("Will try to convert data automatically to new format.");
      }
      this.setNPCDataVersion(npcDataVersion);
    } else {
      log.warn("Legacy Easy NPC Data for {}! Will try to convert data to new format.", this);
      this.setNPCDataVersion(-1);
    }
  }
}
