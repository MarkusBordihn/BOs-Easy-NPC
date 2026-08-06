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
import de.markusbordihn.easynpc.component.DataComponents;
import de.markusbordihn.easynpc.data.attribute.LegacyAttributeConverter;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.security.SecurityManager;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.ProblemReporter;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.SpawnData;
import net.minecraft.world.level.storage.TagValueInput;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PresetDataUtils {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String[] RUNTIME_STATE_TAGS = {
    "AbsorptionAmount",
    "Air",
    "DeathTime",
    "FallDistance",
    "Fire",
    "HurtByTimestamp",
    "HurtTime",
    "Motion",
    "OnGround",
    "PersistenceRequired"
  };
  private static final String[] POSITION_TAGS = {
    "Pos", "Rotation", NavigationDataCapable.DATA_NAVIGATION_TAG
  };
  private static final String ENTITY_UUID_TAG = "UUID";

  private PresetDataUtils() {}

  public static CompoundTag cleanupEntityData(CompoundTag entityData) {
    return cleanupEntityData(entityData, CleanupMode.RUNTIME_ONLY);
  }

  public static CompoundTag cleanupEntityData(CompoundTag entityData, CleanupMode mode) {
    if (entityData == null || entityData.isEmpty()) {
      return entityData;
    }

    for (String tag : RUNTIME_STATE_TAGS) {
      entityData.remove(tag);
    }

    if (mode == CleanupMode.FULL) {
      for (String tag : POSITION_TAGS) {
        entityData.remove(tag);
      }
    }

    return entityData;
  }

  public static SpawnData toSpawnData(PresetData presetData) {
    if (presetData == null || !presetData.hasValidData()) {
      return new SpawnData();
    }

    CompoundTag dataCopy = presetData.data().copy();
    if (CompoundTagUtils.readUUID(dataCopy, ENTITY_UUID_TAG) == null) {
      CompoundTagUtils.writeUUID(dataCopy, ENTITY_UUID_TAG, UUID.randomUUID());
      log.debug("Generated missing Entity UUID in toSpawnData");
    }

    return new SpawnData(dataCopy, Optional.empty(), Optional.empty());
  }

  public static SpawnData toSpawnData(PresetData presetData, Level level, Player player) {
    return toSpawnData(SecurityManager.sanitizePresetDataForSpawn(presetData, level, player));
  }

  public static PresetData fromSpawnData(SpawnData spawnData) {
    if (spawnData == null) {
      return PresetData.EMPTY;
    }

    CompoundTag entityData = spawnData.getEntityToSpawn();
    if (!entityData.contains("id")) {
      return PresetData.EMPTY;
    }

    Optional<String> entityTypeIdOpt = entityData.getString("id");
    if (entityTypeIdOpt.isEmpty()) {
      return PresetData.EMPTY;
    }

    EntityType<?> entityType = EntityType.byString(entityTypeIdOpt.get()).orElse(null);

    if (entityType == null) {
      return PresetData.EMPTY;
    }

    return new PresetData(entityType, entityData.copy());
  }

  public static ItemStack toItemStack(PresetData presetData) {
    if (presetData == null || !presetData.hasValidData()) {
      log.warn("Cannot create item stack from invalid preset data");
      return ItemStack.EMPTY;
    }

    Item item =
        BuiltInRegistries.ITEM
            .getOptional(Identifier.fromNamespaceAndPath(Constants.MOD_ID, "easy_npc_preset"))
            .orElse(null);
    if (item == null) {
      log.error("Cannot find easy_npc_preset item in registry");
      return ItemStack.EMPTY;
    }

    ItemStack itemStack = new ItemStack(item);
    itemStack.set(
        DataComponents.PRESET_DATA,
        new PresetData(
            presetData.entityType(),
            cleanupEntityData(presetData.data().copy(), CleanupMode.FULL)));

    return itemStack;
  }

  public static PresetData fromItemStack(ItemStack itemStack) {
    if (itemStack.isEmpty()) {
      return PresetData.EMPTY;
    }

    PresetData presetData = itemStack.get(DataComponents.PRESET_DATA);

    return presetData != null && presetData.hasValidData() ? presetData : PresetData.EMPTY;
  }

  public static boolean spawnEntity(PresetData presetData, Level level, BlockPos blockPos) {
    return spawnEntity(presetData, level, blockPos, null);
  }

  public static boolean spawnEntity(
      PresetData presetData, Level level, BlockPos blockPos, Player player) {
    if (level.isClientSide()
        || !(level instanceof ServerLevel serverLevel)
        || presetData == null
        || !presetData.hasValidData()) {
      return false;
    }

    PresetData sanitizedPresetData =
        SecurityManager.sanitizePresetDataForSpawn(presetData, level, player);
    Entity entity = sanitizedPresetData.entityType().create(serverLevel, EntitySpawnReason.COMMAND);
    if (entity == null) {
      log.error("Unable to create entity for {} in {}", sanitizedPresetData.entityType(), level);
      return false;
    }

    CompoundTag entityData = sanitizedPresetData.data().copy();
    if (entityData.contains(ENTITY_UUID_TAG)) {
      entityData.remove(ENTITY_UUID_TAG);
    }

    LegacyAttributeConverter.convertLegacyAttributes(entityData);

    if (entity instanceof EasyNPC<?> easyNPC && easyNPC.getEasyNPCPresetData() != null) {
      easyNPC.registerEasyNPCDefaultData();
      easyNPC.getEasyNPCPresetData().importPresetData(entityData);
    } else {
      entity.load(
          TagValueInput.create(
              ProblemReporter.DISCARDING, serverLevel.registryAccess(), entityData));
    }
    entity.setPos(blockPos.getX() + 0.5, blockPos.getY(), blockPos.getZ() + 0.5);

    // Replace the home position carried over from the preset with the actual spawn position.
    if (entity instanceof EasyNPC<?> easyNPC && easyNPC.getEasyNPCNavigationData() != null) {
      easyNPC.getEasyNPCNavigationData().setNPCHomePosition(blockPos);
    }

    if (entity instanceof LivingEntity livingEntity) {
      float maxHealth =
          livingEntity.getAttribute(Attributes.MAX_HEALTH) != null
              ? (float) livingEntity.getAttribute(Attributes.MAX_HEALTH).getValue()
              : 20.0f;
      livingEntity.setHealth(maxHealth);
      livingEntity.deathTime = 0;
      livingEntity.hurtTime = 0;
    }

    if (level.addFreshEntity(entity)) {
      log.debug("Spawned {} at {} in {}", presetData.entityType(), blockPos, level);
      return true;
    }

    return false;
  }

  public enum CleanupMode {
    RUNTIME_ONLY,
    FULL
  }
}
