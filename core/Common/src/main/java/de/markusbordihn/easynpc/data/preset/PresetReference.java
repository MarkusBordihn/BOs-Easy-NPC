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

package de.markusbordihn.easynpc.data.preset;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntityType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PresetReference {

  private static final List<String> IDENTITY_TAGS =
      List.of(
          PresetData.UUID_TAG,
          PresetData.PRESET_UUID_TAG,
          PresetDataCapable.PRESET_METADATA_TAG,
          OwnerDataCapable.DATA_OWNER_TAG,
          NavigationDataCapable.DATA_NAVIGATION_TAG,
          "Pos",
          "Rotation",
          "OnGround");
  private static final Map<EntityType<?>, CompoundTag> referenceTags = new ConcurrentHashMap<>();
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private PresetReference() {}

  public static CompoundTag getReferenceTag(EntityType<?> entityType, ServerLevel serverLevel) {
    if (entityType == null || serverLevel == null) {
      return null;
    }

    CompoundTag referenceTag =
        referenceTags.computeIfAbsent(entityType, type -> createReferenceTag(type, serverLevel));
    return referenceTag != null ? referenceTag.copy() : null;
  }

  public static void clearCache() {
    referenceTags.clear();
  }

  private static CompoundTag createReferenceTag(EntityType<?> entityType, ServerLevel serverLevel) {
    Entity entity = entityType.create(serverLevel, EntitySpawnReason.COMMAND);
    if (!(entity instanceof EasyNPC<?> easyNPC) || easyNPC.getEasyNPCPresetData() == null) {
      if (entity != null) {
        entity.discard();
      }
      log.error("Unable to create a reference NPC for {}", entityType);
      return null;
    }

    try {
      easyNPC.registerEasyNPCDefaultData();

      CompoundTag referenceTag = easyNPC.getEasyNPCPresetData().serializePresetData();
      IDENTITY_TAGS.forEach(referenceTag::remove);
      PresetNormalizer.normalize(referenceTag);
      return referenceTag;
    } finally {
      entity.discard();
    }
  }
}
