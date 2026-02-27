/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.data.npc;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.Mob;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public record SavedNPCEntityEntry(
    UUID entityUUID, CompoundTag npcData, NPCEntityMetadata metadata) {
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String DATA_ENTITY_UUID_TAG = "EntityUUID";
  private static final String DATA_NPC_DATA_TAG = "NPCData";
  private static final String DATA_METADATA_TAG = "Metadata";

  public static <T extends Mob> SavedNPCEntityEntry fromEasyNPC(EasyNPC<T> easyNPC) {
    if (easyNPC == null || easyNPC.getEntity() == null) {
      log.error("Cannot create SavedNPCEntityEntry from null EasyNPC or null entity");
      return null;
    }

    PresetDataCapable<?> presetData = easyNPC.getEasyNPCPresetData();
    if (presetData == null) {
      log.error("Cannot serialize NPC {} - missing PresetDataCapable", easyNPC.getEntityUUID());
      return null;
    }

    NPCEntityMetadata metadata = NPCEntityMetadata.fromEasyNPC(easyNPC);
    return new SavedNPCEntityEntry(
        easyNPC.getEntityUUID(), presetData.serializePresetData(), metadata);
  }

  public static SavedNPCEntityEntry load(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_ENTITY_UUID_TAG)) {
      log.error("Cannot load SavedNPCEntityEntry - missing UUID");
      return null;
    }

    return new SavedNPCEntityEntry(
        compoundTag.getUUID(DATA_ENTITY_UUID_TAG),
        compoundTag.contains(DATA_NPC_DATA_TAG)
            ? compoundTag.getCompound(DATA_NPC_DATA_TAG)
            : new CompoundTag(),
        compoundTag.contains(DATA_METADATA_TAG)
            ? NPCEntityMetadata.fromCompoundTag(compoundTag.getCompound(DATA_METADATA_TAG))
            : NPCEntityMetadata.DEFAULT);
  }

  public CompoundTag save(CompoundTag compoundTag) {
    compoundTag.putUUID(DATA_ENTITY_UUID_TAG, this.entityUUID);
    if (this.npcData != null && !this.npcData.isEmpty()) {
      compoundTag.put(DATA_NPC_DATA_TAG, this.npcData);
    }
    if (this.metadata != null) {
      compoundTag.put(DATA_METADATA_TAG, this.metadata.toCompoundTag());
    }
    return compoundTag;
  }
}
