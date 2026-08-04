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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.preset.BasePresetGenerator;
import de.markusbordihn.easynpc.data.preset.PresetAccess;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetInheritance;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.io.DataFileHandler;
import java.io.File;
import java.util.List;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;

public class BasePresetTestHelper {

  private static final String GENERATED_FOLDER_NAME = "generated";
  private static final String CUSTOM_NAME_TAG = "CustomName";
  private static final String CHILD_PRESET_NAME = "Child Of Base Preset";

  private BasePresetTestHelper() {}

  public static void assertBasePresetExistsForEverySpawnEgg(GameTestHelper helper) {
    List<EntityType<?>> entityTypes = BasePresetGenerator.getBasePresetEntityTypes();
    GameTestHelpers.assertTrue(
        helper, "There must be an NPC type with a spawn egg", !entityTypes.isEmpty());

    for (EntityType<?> entityType : entityTypes) {
      CompoundTag basePreset = BasePresetGenerator.createBasePreset(entityType, helper.getLevel());
      GameTestHelpers.assertNotNull(
          helper, "A base preset must be created for " + EntityType.getKey(entityType), basePreset);
      assertBasePresetStructure(helper, entityType, basePreset);
    }
  }

  public static void assertBasePresetsAreWritten(GameTestHelper helper) {
    File targetFolder =
        DataFileHandler.getCustomDataFolder()
            .resolve(GENERATED_FOLDER_NAME)
            .resolve(DataFileHandler.RESOURCE_BASE_PRESET_PATH)
            .toFile();

    int generatedPresets = BasePresetGenerator.generateBasePresets(helper.getLevel(), targetFolder);

    GameTestHelpers.assertEquals(
        helper,
        "Every NPC type with a spawn egg must get a base preset file",
        BasePresetGenerator.getBasePresetEntityTypes().size(),
        generatedPresets);
  }

  public static void assertShippedBasePresetsAreUpToDate(GameTestHelper helper) {
    for (EntityType<?> entityType : BasePresetGenerator.getBasePresetEntityTypes()) {
      ResourceLocation basePresetLocation = BasePresetGenerator.getBasePresetLocation(entityType);
      CompoundTag shippedBasePreset =
          PresetHandler.loadPresetCompoundTag(
              PresetType.DATA, basePresetLocation, helper.getLevel().getServer());
      GameTestHelpers.assertNotNull(
          helper, "The base preset " + basePresetLocation + " must be shipped", shippedBasePreset);

      CompoundTag generatedBasePreset =
          BasePresetGenerator.createBasePreset(entityType, helper.getLevel());
      GameTestHelpers.assertNotNull(
          helper, "A base preset must be created for " + basePresetLocation, generatedBasePreset);
      GameTestHelpers.assertEquals(
          helper,
          "The shipped base preset "
              + basePresetLocation
              + " is outdated, regenerate it with /easy_npc preset generate",
          generatedBasePreset.getCompound(PresetData.DATA_TAG),
          shippedBasePreset.getCompound(PresetData.DATA_TAG));

      PresetMetadata generatedMetadata =
          PresetMetadata.fromCompoundTag(
              generatedBasePreset.getCompound(PresetDataCapable.PRESET_METADATA_TAG));
      PresetMetadata shippedMetadata =
          PresetMetadata.fromCompoundTag(
              shippedBasePreset.getCompound(PresetDataCapable.PRESET_METADATA_TAG));
      GameTestHelpers.assertEquals(
          helper,
          "The shipped base preset " + basePresetLocation + " must name the generated NPC type",
          generatedMetadata.entityTypeId(),
          shippedMetadata.entityTypeId());
      GameTestHelpers.assertEquals(
          helper,
          "The shipped base preset " + basePresetLocation + " must keep the generated variant",
          String.valueOf(generatedMetadata.variantType()),
          String.valueOf(shippedMetadata.variantType()));
    }
  }

  public static void assertBasePresetIsUsableAsParent(
      GameTestHelper helper, EntityType<?> entityType) {
    ResourceLocation basePresetLocation = BasePresetGenerator.getBasePresetLocation(entityType);
    CompoundTag childPreset = new CompoundTag();
    childPreset.putString(PresetData.PARENT_TAG, basePresetLocation.toString());
    CompoundTag childEntityData = new CompoundTag();
    childEntityData.putString(CUSTOM_NAME_TAG, CHILD_PRESET_NAME);
    childPreset.put(PresetData.DATA_TAG, childEntityData);

    CompoundTag resolvedPreset =
        PresetInheritance.resolve(
            childPreset,
            new ResourceLocation(Constants.MOD_ID, "gametest/child"),
            parentLocation ->
                PresetHandler.loadPresetCompoundTag(
                    PresetType.DATA, parentLocation, helper.getLevel().getServer()));

    GameTestHelpers.assertNotNull(
        helper,
        "A preset which builds on " + basePresetLocation + " must be resolvable",
        resolvedPreset);

    CompoundTag resolvedEntityData = resolvedPreset.getCompound(PresetData.DATA_TAG);
    GameTestHelpers.assertEquals(
        helper,
        "The resolved preset must keep the entity type of its base preset",
        EntityType.getKey(entityType).toString(),
        resolvedEntityData.getString(Entity.ID_TAG));
    GameTestHelpers.assertEquals(
        helper,
        "The resolved preset must keep its own name",
        CHILD_PRESET_NAME,
        resolvedEntityData.getString(CUSTOM_NAME_TAG));
    GameTestHelpers.assertTrue(
        helper,
        "The resolved preset must inherit the objectives of its base preset",
        resolvedEntityData.contains(ObjectiveDataCapable.DATA_OBJECTIVE_DATA_TAG));

    PresetMetadata resolvedMetadata =
        PresetMetadata.fromCompoundTag(
            resolvedPreset.getCompound(PresetDataCapable.PRESET_METADATA_TAG));
    GameTestHelpers.assertEquals(
        helper,
        "A preset which builds on a base preset must not inherit its hidden access",
        PresetAccess.PUBLIC,
        resolvedMetadata.access());
  }

  private static void assertBasePresetStructure(
      GameTestHelper helper, EntityType<?> entityType, CompoundTag basePreset) {
    String entityTypeId = EntityType.getKey(entityType).toString();

    GameTestHelpers.assertTrue(
        helper,
        "The base preset of " + entityTypeId + " must use the wrapped format",
        PresetData.usesEntityDataWrapper(basePreset));

    CompoundTag entityData = basePreset.getCompound(PresetData.DATA_TAG);
    GameTestHelpers.assertEquals(
        helper,
        "The base preset of " + entityTypeId + " must name its entity type",
        entityTypeId,
        entityData.getString(Entity.ID_TAG));

    for (String identityTag :
        List.of(Entity.UUID_TAG, PresetData.PRESET_UUID_TAG, "Pos", "Owner")) {
      GameTestHelpers.assertTrue(
          helper,
          "The base preset of " + entityTypeId + " must not carry " + identityTag,
          !entityData.contains(identityTag));
    }

    PresetMetadata metadata =
        PresetMetadata.fromCompoundTag(
            basePreset.getCompound(PresetDataCapable.PRESET_METADATA_TAG));
    GameTestHelpers.assertEquals(
        helper,
        "The base preset of " + entityTypeId + " must stay hidden",
        PresetAccess.INTERNAL,
        metadata.access());
    GameTestHelpers.assertEquals(
        helper,
        "The base preset of " + entityTypeId + " must name its entity type in the metadata",
        entityTypeId,
        metadata.entityTypeId());
  }
}
