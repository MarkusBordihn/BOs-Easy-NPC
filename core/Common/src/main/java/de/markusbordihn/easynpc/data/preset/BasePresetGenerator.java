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
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import de.markusbordihn.easynpc.io.DataFileHandler;
import de.markusbordihn.easynpc.io.PresetFileHandler;
import de.markusbordihn.easynpc.item.ModSpawnEggItem;
import java.io.File;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class BasePresetGenerator {

  public static final String BASE_PRESET_CATEGORY = "Base";
  public static final String BASE_PRESET_AUTHOR = "Easy NPC";
  public static final String BASE_PRESET_NAME_SUFFIX = " Base";

  private static final long GENERATED_TIMESTAMP = 1L;

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private BasePresetGenerator() {}

  public static List<EntityType<?>> getBasePresetEntityTypes() {
    List<EntityType<?>> entityTypes = new ArrayList<>();
    for (EntityType<?> entityType : BuiltInRegistries.ENTITY_TYPE) {
      if (hasSpawnEgg(entityType)) {
        entityTypes.add(entityType);
      }
    }
    entityTypes.sort(Comparator.comparing(entityType -> EntityType.getKey(entityType).toString()));

    return entityTypes;
  }

  public static ResourceLocation getBasePresetLocation(EntityType<?> entityType) {
    ResourceLocation entityTypeId = EntityType.getKey(entityType);
    return ResourceLocation.fromNamespaceAndPath(
        entityTypeId.getNamespace(),
        DataFileHandler.RESOURCE_BASE_PRESET_PATH
            + "/"
            + entityTypeId.getPath()
            + PresetExportFormat.SNBT.getFileExtension());
  }

  public static CompoundTag createBasePreset(EntityType<?> entityType, ServerLevel serverLevel) {
    CompoundTag referenceTag = PresetReference.getReferenceTag(entityType, serverLevel);
    if (referenceTag == null) {
      return null;
    }

    CompoundTag basePreset = new CompoundTag();
    basePreset.put(
        PresetDataCapable.PRESET_METADATA_TAG,
        createBasePresetMetadata(entityType, serverLevel).toCompoundTag());
    basePreset.put(PresetData.DATA_TAG, referenceTag);

    return basePreset;
  }

  public static int generateBasePresets(ServerLevel serverLevel, File targetFolder) {
    if (serverLevel == null || targetFolder == null) {
      return 0;
    }

    if (!targetFolder.exists() && !targetFolder.mkdirs()) {
      log.error("Unable to create the base preset folder {}", targetFolder);
      return 0;
    }

    int generatedPresets = 0;
    for (EntityType<?> entityType : getBasePresetEntityTypes()) {
      CompoundTag basePreset = createBasePreset(entityType, serverLevel);
      if (basePreset == null) {
        continue;
      }

      File presetFile =
          new File(
              targetFolder,
              EntityType.getKey(entityType).getPath() + PresetExportFormat.SNBT.getFileExtension());
      if (PresetFileHandler.save(presetFile, basePreset)) {
        generatedPresets++;
      }
    }

    log.info("Generated {} base presets in {}", generatedPresets, targetFolder);
    return generatedPresets;
  }

  private static PresetMetadata createBasePresetMetadata(
      EntityType<?> entityType, ServerLevel serverLevel) {
    ResourceLocation entityTypeId = EntityType.getKey(entityType);

    return new PresetMetadata(
        entityType.getDescription().getString() + BASE_PRESET_NAME_SUFFIX,
        BASE_PRESET_CATEGORY,
        PresetMetadata.DEFAULT_VERSION,
        BASE_PRESET_AUTHOR,
        GENERATED_TIMESTAMP,
        GENERATED_TIMESTAMP,
        "",
        entityTypeId.toString(),
        getVariantType(entityType, serverLevel),
        PresetAccess.INTERNAL);
  }

  private static String getVariantType(EntityType<?> entityType, ServerLevel serverLevel) {
    Entity entity = entityType.create(serverLevel);
    if (!(entity instanceof VariantDataCapable<?> variantData)) {
      if (entity != null) {
        entity.discard();
      }
      return null;
    }

    try {
      Enum<?> variant = variantData.getSkinVariantType();
      return variant != null ? variant.name() : null;
    } finally {
      entity.discard();
    }
  }

  private static boolean hasSpawnEgg(EntityType<?> entityType) {
    ResourceLocation entityTypeId = EntityType.getKey(entityType);
    if (!Constants.MOD_ID.equals(entityTypeId.getNamespace())) {
      return false;
    }

    return BuiltInRegistries.ITEM.containsKey(
        ResourceLocation.fromNamespaceAndPath(
            entityTypeId.getNamespace(), entityTypeId.getPath() + ModSpawnEggItem.SUFFIX));
  }
}
