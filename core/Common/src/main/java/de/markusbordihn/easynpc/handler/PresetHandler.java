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

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetExportFormat;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.io.PresetFileHandler;
import de.markusbordihn.easynpc.io.WorldPresetDataFiles;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.UUID;
import java.util.function.BiFunction;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtAccounter;
import net.minecraft.nbt.NbtIo;
import net.minecraft.nbt.TagParser;
import net.minecraft.resources.Identifier;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PresetHandler {

  public static final String UUID_TAG = "UUID";
  public static final String ID_TAG = "id";
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private PresetHandler() {}

  public static boolean importPreset(
      ServerLevel serverLevel,
      PresetType presetType,
      Identifier presetLocation,
      Vec3 position,
      UUID uuid,
      ServerPlayer serverPlayer) {
    PresetData presetData =
        loadPresetFromSource(presetType, presetLocation, serverLevel.getServer());
    return presetData != null
        && importPreset(serverLevel, presetData, position, uuid, serverPlayer);
  }

  public static boolean importPreset(
      ServerLevel serverLevel,
      PresetData presetData,
      Vec3 position,
      UUID uuid,
      ServerPlayer serverPlayer) {
    if (presetData == null || !presetData.hasValidData()) {
      log.error("[{}] Invalid preset data for import", serverLevel);
      return false;
    }

    PresetData updatedPresetData = presetData;
    if (position != null) {
      updatedPresetData = updatedPresetData.withPosition(position);
    }
    if (uuid != null) {
      updatedPresetData = updatedPresetData.withUUID(uuid);
    }

    if (!importPreset(serverLevel, updatedPresetData.data())) {
      return false;
    }

    UUID finalUuid =
        uuid != null ? uuid : CompoundTagUtils.readUUID(updatedPresetData.data(), UUID_TAG);
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(finalUuid, serverLevel);
    if (easyNPC == null) {
      log.error("[{}] Error importing preset, no entity found for {}", serverLevel, finalUuid);
      return false;
    }

    configureImportedEntity(easyNPC, position, serverPlayer);
    return true;
  }

  private static void configureImportedEntity(
      EasyNPC<?> easyNPC, Vec3 position, ServerPlayer serverPlayer) {
    if (serverPlayer != null) {
      OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
      if (ownerData != null) {
        ownerData.setNPCOwner(serverPlayer);
      }
    }

    if (position != null) {
      NavigationDataCapable<?> navigationData = easyNPC.getEasyNPCNavigationData();
      if (navigationData != null && !easyNPC.getEntity().position().equals(position)) {
        navigationData.setHomePosition(
            new BlockPos((int) position.x, (int) position.y, (int) position.z));
      }
    }
  }

  public static boolean importPreset(ServerLevel serverLevel, CompoundTag compoundTag) {
    if (!validateImportParameters(serverLevel, compoundTag)) {
      return false;
    }

    EntityType<?> entityType = validateAndGetEntityType(compoundTag, serverLevel);
    if (entityType == null) {
      return false;
    }

    UUID existingUUID = CompoundTagUtils.readUUID(compoundTag, UUID_TAG);
    if (existingUUID != null && tryUpdateExistingEntity(existingUUID, compoundTag, serverLevel)) {
      return true;
    }

    return createAndImportNewEntity(entityType, compoundTag, serverLevel);
  }

  private static boolean validateImportParameters(
      ServerLevel serverLevel, CompoundTag compoundTag) {
    if (serverLevel == null || compoundTag == null) {
      log.error("[{}] Error importing preset ", serverLevel);
      return false;
    }

    if (compoundTag.isEmpty()) {
      log.error("[{}] Empty preset data for import", serverLevel);
      return false;
    }

    return true;
  }

  private static EntityType<?> validateAndGetEntityType(
      CompoundTag compoundTag, ServerLevel serverLevel) {
    if (!compoundTag.contains(ID_TAG)) {
      log.error("[{}] Error importing preset, missing entity type", serverLevel);
      return null;
    }

    EntityType<?> entityType =
        EntityType.byString(compoundTag.getString(ID_TAG).orElse("")).orElse(null);
    if (entityType == null) {
      log.error("[{}] Error importing preset, invalid entity type", serverLevel);
    }

    return entityType;
  }

  private static boolean tryUpdateExistingEntity(
      UUID uuid, CompoundTag compoundTag, ServerLevel serverLevel) {
    EasyNPC<?> existingEasyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverLevel);
    if (existingEasyNPC == null) {
      return false;
    }

    String entityTypeId = compoundTag.getString(ID_TAG).orElse("");
    if (compoundTag.contains(ID_TAG)
        && !entityTypeId.isEmpty()
        && entityTypeId.equals(existingEasyNPC.getEntityTypeId())
        && existingEasyNPC.getEasyNPCPresetData() != null) {
      log.debug("[{}] Update preset data for existing entity {}!", serverLevel, existingEasyNPC);
      existingEasyNPC.getEasyNPCPresetData().importPresetData(compoundTag);
      return true;
    }

    LivingEntityManager.discardEasyNPCEntityByUUID(uuid, serverLevel);
    return false;
  }

  private static boolean createAndImportNewEntity(
      EntityType<?> entityType, CompoundTag compoundTag, ServerLevel serverLevel) {
    Entity entity = entityType.create(serverLevel, EntitySpawnReason.SPAWN_ITEM_USE);
    if (entity == null) {
      log.error("[{}] Failed to create entity of type {}", serverLevel, entityType);
      return false;
    }

    if (!(entity instanceof EasyNPC<?> easyNPCEntity)) {
      entity.discard();
      log.error("[{}] Entity type {} is not an EasyNPC", serverLevel, entityType);
      return false;
    }

    PresetDataCapable<?> presetData = easyNPCEntity.getEasyNPCPresetData();
    if (presetData == null) {
      entity.discard();
      log.error("[{}] No preset data available for {}", serverLevel, easyNPCEntity);
      return false;
    }

    try {
      presetData.importPresetData(compoundTag);
      if (!serverLevel.addFreshEntity(easyNPCEntity.getEntity())) {
        entity.discard();
        log.error("[{}] Error spawning entity", easyNPCEntity);
        return false;
      }
      log.debug("[{}] Imported preset data {} for {}", serverLevel, compoundTag, easyNPCEntity);
      return true;
    } catch (Exception e) {
      entity.discard();
      log.error("[{}] Error importing preset data", serverLevel, e);
      return false;
    }
  }

  private static PresetData loadPresetFromSource(
      PresetType presetType, Identifier presetLocation, MinecraftServer minecraftServer) {
    if (presetLocation == null || minecraftServer == null) {
      return null;
    }

    CompoundTag compoundTag =
        switch (presetType) {
          case CUSTOM ->
              loadFromFile(
                  CustomPresetDataFiles.getPresetsIdentifierPath(presetLocation), presetLocation);
          case WORLD ->
              loadFromFile(
                  WorldPresetDataFiles.getPresetsIdentifierPath(presetLocation), presetLocation);
          case DATA, DEFAULT -> {
            try {
              var resource = minecraftServer.getResourceManager().getResource(presetLocation);
              if (resource.isEmpty()) {
                log.error("{} preset resource not found at {}", presetType, presetLocation);
                yield null;
              }
              try (var inputStream = resource.get().open()) {
                PresetExportFormat format =
                    PresetExportFormat.getPresetExportFormat(presetLocation.getPath());
                if (format == PresetExportFormat.SNBT) {
                  String content = new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
                  yield TagParser.parseCompoundFully(content);
                } else if (format == PresetExportFormat.NBT) {
                  yield NbtIo.readCompressed(inputStream, NbtAccounter.unlimitedHeap());
                } else {
                  log.error("Unknown preset format for {}", presetLocation);
                  yield null;
                }
              }
            } catch (IOException exception) {
              log.error(
                  "Error reading {} preset resource {}", presetType, presetLocation, exception);
              yield null;
            } catch (CommandSyntaxException exception) {
              log.error(
                  "Error parsing SNBT {} preset resource {}",
                  presetType,
                  presetLocation,
                  exception);
              yield null;
            }
          }
          default -> {
            log.error("Unsupported preset type for loading: {}", presetType);
            yield null;
          }
        };

    if (compoundTag == null) {
      return null;
    }

    return PresetData.fromCompoundTag(presetLocation, presetType, compoundTag);
  }

  private static CompoundTag loadFromFile(Path presetFile, Identifier presetLocation) {
    if (presetFile == null) {
      log.error("Preset file path is null for: {}", presetLocation);
      return null;
    }
    return PresetFileHandler.load(presetFile.toFile());
  }

  @SuppressWarnings("unused")
  public static boolean importLocalPreset(
      ServerLevel serverLevel,
      CompoundTag compoundTag,
      Identifier presetLocation,
      Vec3 position,
      UUID uuid,
      ServerPlayer serverPlayer) {
    if (serverLevel == null || presetLocation == null) {
      log.error("[{}] Error importing local preset {}", serverLevel, presetLocation);
      return false;
    }

    if (compoundTag == null || compoundTag.isEmpty()) {
      log.error(
          "[{}] Error importing local preset {}, no preset data found!",
          serverLevel,
          presetLocation);
      return false;
    }

    PresetData presetData =
        PresetData.fromCompoundTag(presetLocation, PresetType.LOCAL, compoundTag);
    if (presetData == null || !presetData.hasValidData()) {
      log.error("[{}] Error converting local preset to PresetData", serverLevel);
      return false;
    }

    return importPreset(serverLevel, presetData, position, uuid, serverPlayer);
  }

  public static boolean exportCustomPreset(EasyNPC<?> easyNPC, String name) {
    return exportPresetByType(easyNPC, name, CustomPresetDataFiles::getPresetFile);
  }

  public static boolean exportWorldPreset(EasyNPC<?> easyNPC, String name) {
    return exportPresetByType(easyNPC, name, WorldPresetDataFiles::getPresetFile);
  }

  private static boolean exportPresetByType(
      EasyNPC<?> easyNPC, String name, BiFunction<SkinModel, String, File> fileProvider) {
    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData == null) {
      log.warn("[{}] Error no skin data available!", easyNPC);
      return false;
    }

    File presetFile = fileProvider.apply(skinData.getSkinModel(), name);
    return exportPreset(easyNPC, presetFile);
  }

  public static boolean exportPreset(EasyNPC<?> easyNPC, File file) {
    if (easyNPC == null || file == null) {
      log.error("[{}] Error exporting preset {} !", easyNPC, file);
      return false;
    }

    PresetDataCapable<?> presetData = easyNPC.getEasyNPCPresetData();
    if (presetData == null) {
      log.error("[{}] Error no preset data available!", easyNPC);
      return false;
    }

    CompoundTag compoundTag = presetData.serializePresetData();
    if (compoundTag == null || compoundTag.isEmpty()) {
      log.error("[{}] Error exporting custom preset {}!", easyNPC, file);
      return false;
    }

    return PresetFileHandler.save(file, compoundTag);
  }

  @SuppressWarnings("unused")
  public static CompoundTag prepareClientExportData(EasyNPC<?> easyNPC, PresetMetadata metadata) {
    if (easyNPC == null) {
      log.error("Cannot prepare client export data, easyNPC is null");
      return null;
    }

    CompoundTag presetData = serializeAndCopyPresetData(easyNPC);
    if (presetData == null) {
      return null;
    }

    PresetMetadata finalMetadata = extractAndEnrichMetadata(presetData, metadata, easyNPC);
    presetData.remove(PresetDataCapable.PRESET_METADATA_TAG);

    CompoundTag wrapper = new CompoundTag();
    wrapper.put(PresetDataCapable.PRESET_METADATA_TAG, finalMetadata.toCompoundTag());
    wrapper.put("data", presetData);

    log.debug(
        "[{}] Prepared client export data with metadata: {}",
        easyNPC.getEntity().getName().getString(),
        finalMetadata.category());

    return wrapper;
  }

  private static CompoundTag serializeAndCopyPresetData(EasyNPC<?> easyNPC) {
    PresetDataCapable<?> presetDataCapable = easyNPC.getEasyNPCPresetData();
    if (presetDataCapable == null) {
      log.error("[{}] No preset data available!", easyNPC);
      return null;
    }

    CompoundTag originalPresetData = presetDataCapable.serializePresetData();
    if (originalPresetData == null || originalPresetData.isEmpty()) {
      log.error("[{}] Error serializing preset data!", easyNPC);
      return null;
    }

    return originalPresetData.copy();
  }

  private static PresetMetadata extractAndEnrichMetadata(
      CompoundTag presetData, PresetMetadata providedMetadata, EasyNPC<?> easyNPC) {
    PresetMetadata metadata = providedMetadata;

    if (presetData.contains(PresetDataCapable.PRESET_METADATA_TAG)) {
      CompoundTag metadataTag =
          presetData.getCompound(PresetDataCapable.PRESET_METADATA_TAG).orElse(new CompoundTag());
      PresetMetadata extractedMetadata = PresetMetadata.fromCompoundTag(metadataTag);
      metadata = providedMetadata != null ? providedMetadata : extractedMetadata;
    } else if (metadata == null) {
      metadata = PresetMetadata.createDefault();
    }

    if (metadata.entityTypeId() == null || metadata.variantType() == null) {
      String entityTypeId = easyNPC.getEntityTypeId();
      String variantType = extractVariantType(easyNPC);
      metadata = metadata.withPreviewData(entityTypeId, variantType);
    }

    return metadata;
  }

  private static String extractVariantType(EasyNPC<?> easyNPC) {
    if (easyNPC
        instanceof de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable<?> variantData) {
      Enum<?> variant = variantData.getSkinVariantType();
      if (variant != null) {
        return variant.name();
      }
    }
    return null;
  }

  @SuppressWarnings("unused")
  public static PresetData loadPreset(
      Identifier presetLocation, PresetType presetType, MinecraftServer server) {
    if (presetLocation == null || server == null) {
      log.error("Cannot load preset, location or server is null");
      return null;
    }
    return loadPresetFromSource(presetType, presetLocation, server);
  }
}
