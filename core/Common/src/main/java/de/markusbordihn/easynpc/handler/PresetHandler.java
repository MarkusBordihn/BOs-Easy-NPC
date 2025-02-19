/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.debug.Logger;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationData;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.io.WorldPresetDataFiles;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.DoubleTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtIo;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.UUID;

public class PresetHandler {


  private PresetHandler() {}

  public static boolean importPreset(
      ServerLevel serverLevel,
      CompoundTag compoundTag,
      Vec3 position,
      UUID uuid,
      ServerPlayer serverPlayer) {

    // Overwrite spawn position, if provided.
    if (position != null) {
      ListTag posTag = new ListTag();
      posTag.add(DoubleTag.valueOf(position.x));
      posTag.add(DoubleTag.valueOf(position.y));
      posTag.add(DoubleTag.valueOf(position.z));
      compoundTag.put("Pos", posTag);
    }

    // Overwrite UUID, if UUID is given.
    if (uuid != null) {
      compoundTag.putUUID(Entity.UUID_TAG, uuid);
    }

    // Import preset data
    if (!importPreset(serverLevel, compoundTag)) {
      return false;
    }

    // Get EasyNPC entity
    UUID compoundUUID = compoundTag.getUUID(Entity.UUID_TAG);
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(compoundUUID, serverLevel);
    if (easyNPC == null) {
      Logger.INSTANCE.error(
          "[{}] Error importing preset {}, no entity found for {}",
          serverLevel,
          compoundTag,
          compoundUUID);
      return false;
    }

    // Set owner, if owner is provided.
    OwnerData<?> ownerData = easyNPC.getEasyNPCOwnerData();
    if (serverPlayer != null && ownerData != null) {
      ownerData.setOwner(serverPlayer);
    }

    // Set home position, if spawn position was provided.
    if (position != null) {
      NavigationData<?> navigationData = easyNPC.getEasyNPCNavigationData();
      if (navigationData == null) {
        Logger.INSTANCE.warn(
            "[{}] Warning: Importing preset, no navigation data available for {}",
            serverLevel,
            easyNPC);
      } else if (!easyNPC.getEntity().position().equals(position)) {
        navigationData.setHomePosition(
            new BlockPos((int) position.x, (int) position.y, (int) position.z));
      }
    }

    Logger.INSTANCE.debug("[{}] Imported preset data {} for {}", serverLevel, compoundUUID, easyNPC);
    return true;
  }

  public static boolean importPreset(ServerLevel serverLevel, CompoundTag compoundTag) {
    if (serverLevel == null || compoundTag == null) {
      Logger.INSTANCE.error("[{}] Error importing preset ", serverLevel);
      return false;
    }

    // Validate preset data
    if (compoundTag.isEmpty()) {
      Logger.INSTANCE.error("[{}] Empty preset data for import", serverLevel);
      return false;
    }

    // Validate entity type
    EntityType<?> entityType =
        compoundTag.contains(Entity.ID_TAG)
            ? EntityType.byString(compoundTag.getString(Entity.ID_TAG)).orElse(null)
            : null;
    if (entityType == null) {
      Logger.INSTANCE.error("[{}] Error importing preset, invalid entity type", serverLevel);
      return false;
    }

    // Get UUID from compound tag and check if entity with this UUID already exists.
    UUID existingUUID =
        compoundTag.contains(Entity.UUID_TAG) ? compoundTag.getUUID(Entity.UUID_TAG) : null;
    if (existingUUID != null
        && LivingEntityManager.getEasyNPCEntityByUUID(existingUUID, serverLevel) != null) {
      EasyNPC<?> existingEasyNPC =
          LivingEntityManager.getEasyNPCEntityByUUID(existingUUID, serverLevel);
      if (compoundTag.contains(Entity.ID_TAG)
          && !compoundTag.getString(Entity.ID_TAG).isEmpty()
          && compoundTag.getString(Entity.ID_TAG).equals(existingEasyNPC.getEntityTypeId())
          && existingEasyNPC.getEasyNPCPresetData() != null) {
        Logger.INSTANCE.debug("[{}] Update preset data for existing entity {}!", serverLevel, existingEasyNPC);
        existingEasyNPC.getEasyNPCPresetData().importPresetData(compoundTag);
        return true;
      } else {
        LivingEntityManager.discardEasyNPCEntityByUUID(existingUUID, serverLevel);
      }
    }

    // Create new entity or re-use existing entity.
    Entity entity = entityType.create(serverLevel);
    if (!(entity instanceof EasyNPC<?> easyNPCEntity)) {
      Logger.INSTANCE.error(
          "[{}] Error importing preset, invalid entity with type {}", serverLevel, entityType);
      return false;
    }

    // Import preset data
    PresetData<?> presetData = easyNPCEntity.getEasyNPCPresetData();
    if (presetData == null) {
      Logger.INSTANCE.error(
          "[{}] Error importing preset, no preset data available for {}",
          serverLevel,
          easyNPCEntity);
      return false;
    }
    presetData.importPresetData(compoundTag);

    // Spawn EasyNPC entity
    if (!serverLevel.addFreshEntity(easyNPCEntity.getEntity())) {
      Logger.INSTANCE.error("[{}] Error spawning entity", easyNPCEntity);
      return false;
    }

    Logger.INSTANCE.debug("[{}] Imported preset data {} for {}", serverLevel, compoundTag, easyNPCEntity);
    return true;
  }

  public static boolean importCustomPreset(
      ServerLevel serverLevel,
      ResourceLocation presetLocation,
      Vec3 position,
      UUID uuid,
      ServerPlayer serverPlayer) {
    if (serverLevel == null || presetLocation == null) {
      Logger.INSTANCE.error("[{}] Error importing custom preset ", serverLevel);
      return false;
    }

    Path presetFile = CustomPresetDataFiles.getPresetsResourceLocationPath(presetLocation);
    if (presetFile == null || !presetFile.toFile().exists()) {
      Logger.INSTANCE.error(
          "[{}] Error importing custom preset, no preset file found at {}",
          serverLevel,
          presetLocation);
      return false;
    }

    try {
      CompoundTag compoundTag = NbtIo.readCompressed(presetFile.toFile());
      return importPreset(serverLevel, compoundTag, position, uuid, serverPlayer);
    } catch (IOException exception) {
      Logger.INSTANCE.error("[{}] Error reading custom preset file {}", serverLevel, presetFile, exception);
      return false;
    }
  }

  public static boolean exportCustomPreset(EasyNPC<?> easyNPC, String name) {
    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData == null) {
      Logger.INSTANCE.warn("[{}] Error no skin data available!", easyNPC);
      return false;
    }

    File presetFile = CustomPresetDataFiles.getPresetFile(skinData.getSkinModel(), name);
    return exportPreset(easyNPC, presetFile);
  }

  public static boolean importDataPreset(
      ServerLevel serverLevel,
      ResourceLocation presetLocation,
      Vec3 position,
      UUID uuid,
      ServerPlayer serverPlayer) {
    if (serverLevel == null || presetLocation == null) {
      Logger.INSTANCE.error("[{}] Error importing data preset ", serverLevel);
      return false;
    }

    MinecraftServer minecraftServer = serverLevel.getServer();
    if (minecraftServer.getResourceManager().getResource(presetLocation).isEmpty()) {
      Logger.INSTANCE.error(
          "[{}] Error importing data preset, no preset file found at {}",
          serverLevel,
          presetLocation);
      return false;
    }

    try {
      CompoundTag compoundTag =
          NbtIo.readCompressed(minecraftServer.getResourceManager().open(presetLocation));
      return importPreset(serverLevel, compoundTag, position, uuid, serverPlayer);
    } catch (IOException exception) {
      Logger.INSTANCE.error("[{}] Error reading data preset file {}", serverLevel, presetLocation, exception);
      return false;
    }
  }

  public static boolean importDefaultPreset(
      ServerLevel serverLevel,
      ResourceLocation presetLocation,
      Vec3 position,
      UUID uuid,
      ServerPlayer serverPlayer) {
    if (serverLevel == null || presetLocation == null) {
      Logger.INSTANCE.error("[{}] Error importing default preset ", serverLevel);
      return false;
    }

    MinecraftServer minecraftServer = serverLevel.getServer();
    if (minecraftServer.getResourceManager().getResource(presetLocation).isEmpty()) {
      Logger.INSTANCE.error(
          "[{}] Error importing data preset, no preset file found at {}",
          serverLevel,
          presetLocation);
      return false;
    }

    try {
      CompoundTag compoundTag =
          NbtIo.readCompressed(minecraftServer.getResourceManager().open(presetLocation));
      return importPreset(serverLevel, compoundTag, position, uuid, serverPlayer);
    } catch (IOException exception) {
      Logger.INSTANCE.error(
          "[{}] Error reading default preset file {}", serverLevel, presetLocation, exception);
      return false;
    }
  }

  public static boolean importLocalPreset(
      ServerLevel serverLevel,
      CompoundTag compoundTag,
      ResourceLocation presetLocation,
      Vec3 position,
      UUID uuid,
      ServerPlayer serverPlayer) {
    if (serverLevel == null || presetLocation == null) {
      Logger.INSTANCE.error("[{}] Error importing local preset {}", serverLevel, presetLocation);
      return false;
    }

    if (compoundTag == null || compoundTag.isEmpty()) {
      Logger.INSTANCE.error(
          "[{}] Error importing local preset {}, no preset data found!",
          serverLevel,
          presetLocation);
      return false;
    }

    if (importPreset(serverLevel, compoundTag, position, uuid, serverPlayer)) {
      return true;
    }

    Logger.INSTANCE.error("[{}] Error reading data preset file {}", serverLevel, presetLocation);
    return false;
  }

  public static boolean importWorldPreset(
      ServerLevel serverLevel,
      ResourceLocation presetLocation,
      Vec3 position,
      UUID uuid,
      ServerPlayer serverPlayer) {
    if (serverLevel == null || presetLocation == null) {
      Logger.INSTANCE.error("[{}] Error importing world preset ", serverLevel);
      return false;
    }

    Path presetFile = WorldPresetDataFiles.getPresetsResourceLocationPath(presetLocation);
    if (presetFile == null || !presetFile.toFile().exists()) {
      Logger.INSTANCE.error(
          "[{}] Error importing world preset, no preset file found at {}",
          serverLevel,
          presetLocation);
      return false;
    }

    try {
      CompoundTag compoundTag = NbtIo.readCompressed(presetFile.toFile());
      return importPreset(serverLevel, compoundTag, position, uuid, serverPlayer);
    } catch (IOException exception) {
      Logger.INSTANCE.error("[{}] Error reading world preset file {}", serverLevel, presetFile, exception);
      return false;
    }
  }

  public static boolean exportWorldPreset(EasyNPC<?> easyNPC, String name) {
    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData == null) {
      Logger.INSTANCE.warn("[{}] Error no skin data available!", easyNPC);
      return false;
    }

    File presetFile = WorldPresetDataFiles.getPresetFile(skinData.getSkinModel(), name);
    return exportPreset(easyNPC, presetFile);
  }

  public static boolean exportPreset(EasyNPC<?> easyNPC, File file) {
    if (easyNPC == null || file == null) {
      Logger.INSTANCE.error("[{}] Error exporting preset {} !", easyNPC, file);
      return false;
    }

    PresetData<?> presetData = easyNPC.getEasyNPCPresetData();
    if (presetData == null) {
      Logger.INSTANCE.error("[{}] Error no preset data available!", easyNPC);
      return false;
    }

    CompoundTag compoundTag = presetData.exportPresetData();
    if (compoundTag == null || compoundTag.isEmpty()) {
      Logger.INSTANCE.error("[{}] Error exporting custom preset {}!", easyNPC, file);
      return false;
    }

    return exportPreset(file, compoundTag);
  }

  public static boolean exportPreset(File file, CompoundTag compoundTag) {
    if (file == null || compoundTag == null) {
      Logger.INSTANCE.error("Error exporting preset file {} with {} !", file, compoundTag);
      return false;
    }

    if (compoundTag.isEmpty()) {
      Logger.INSTANCE.error("Empty preset data for export to {} !", file);
      return false;
    }

    try {
      NbtIo.writeCompressed(compoundTag, file);
      return true;
    } catch (IOException exception) {
      Logger.INSTANCE.error("Failed to export preset file {} with {}:", file, compoundTag, exception);
      return false;
    }
  }
}
