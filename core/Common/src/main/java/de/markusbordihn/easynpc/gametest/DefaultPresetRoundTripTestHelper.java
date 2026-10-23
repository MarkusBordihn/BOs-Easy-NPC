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

import de.markusbordihn.easynpc.api.preset.PresetValidationContext;
import de.markusbordihn.easynpc.api.preset.PresetValidationReport;
import de.markusbordihn.easynpc.api.preset.PresetValidator;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetExportFormat;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.handler.PresetImportResult;
import de.markusbordihn.easynpc.io.DataFileHandler;
import de.markusbordihn.easynpc.io.DefaultPresetDataFiles;
import de.markusbordihn.easynpc.io.PresetFileHandler;
import de.markusbordihn.easynpc.security.CommandSecurity;
import java.io.File;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class DefaultPresetRoundTripTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final String GENERATED_FOLDER_NAME = "generated";
  private static final String DEFAULT_PRESET_FOLDER_NAME = "default_preset";

  private DefaultPresetRoundTripTestHelper() {}

  public static void assertDefaultPresetsAreValid(GameTestHelper helper) {
    List<Identifier> presetLocations = defaultPresetLocations(helper);
    GameTestHelpers.assertTrue(
        helper, "There must be a shipped default preset", !presetLocations.isEmpty());

    for (Identifier presetLocation : presetLocations) {
      CompoundTag presetTag = loadPreset(helper, presetLocation);
      GameTestHelpers.assertNotNull(
          helper, "The default preset " + presetLocation + " must be readable", presetTag);

      PresetValidationReport report =
          PresetValidator.validate(presetTag, PresetValidationContext.offline());
      GameTestHelpers.assertTrue(
          helper,
          "The default preset " + presetLocation + " must be valid: " + report.formatIssues(),
          report.isValid());
    }
  }

  public static void assertDefaultPresetsSurviveRegeneration(GameTestHelper helper) {
    File targetFolder =
        DataFileHandler.getCustomDataFolder()
            .resolve(GENERATED_FOLDER_NAME)
            .resolve(DEFAULT_PRESET_FOLDER_NAME)
            .toFile();
    if (!targetFolder.exists() && !targetFolder.mkdirs()) {
      helper.fail("Unable to create the default preset folder " + targetFolder);
      return;
    }

    for (Identifier presetLocation : defaultPresetLocations(helper)) {
      CompoundTag presetTag = loadPreset(helper, presetLocation);
      if (presetTag == null) {
        continue;
      }

      EasyNPC<?> easyNPC = importPreset(helper, presetTag);
      if (easyNPC == null) {
        continue;
      }

      CompoundTag compactPreset = PresetHandler.prepareExportData(easyNPC);
      GameTestHelpers.assertNotNull(
          helper, "The default preset " + presetLocation + " must be exportable", compactPreset);

      CompoundTag sharedPreset = wrapAsPreset(presetTag, compactPreset);
      EasyNPC<?> reimportedNPC = importPreset(helper, sharedPreset);
      GameTestHelpers.assertNotNull(
          helper,
          "The compact default preset " + presetLocation + " must be importable",
          reimportedNPC);

      CompoundTag reExportedPreset =
          wrapAsPreset(presetTag, PresetHandler.prepareExportData(reimportedNPC));
      GameTestHelpers.assertEquals(
          helper,
          "The compact default preset " + presetLocation + " must produce the same NPC",
          withoutRegeneratedMetadata(sharedPreset.getCompoundOrEmpty(PresetData.DATA_TAG)),
          withoutRegeneratedMetadata(reExportedPreset.getCompoundOrEmpty(PresetData.DATA_TAG)));

      writePreset(helper, targetFolder, presetLocation, sharedPreset);
      easyNPC.getEntity().discard();
      reimportedNPC.getEntity().discard();
    }
  }

  public static void assertDefaultPresetsSpawnAndKeepRunning(GameTestHelper helper) {
    Map<Identifier, EasyNPC<?>> spawnedNPCs = new LinkedHashMap<>();
    for (Identifier presetLocation : defaultPresetLocations(helper)) {
      CompoundTag presetTag = loadPreset(helper, presetLocation);
      GameTestHelpers.assertNotNull(
          helper, "The default preset " + presetLocation + " must be readable", presetTag);
      CompoundTag entityData = entityData(presetTag);

      UUID entityUUID = UUID.randomUUID();
      GameTestHelpers.removeNPCDataAfterTest(helper, entityUUID);
      PresetImportResult importResult =
          PresetHandler.importPresetWithReport(
              helper.getLevel(),
              PresetType.DEFAULT,
              presetLocation,
              helper.absoluteVec(NPC_POSITION),
              entityUUID,
              CommandSecurity.getServerActorContext(),
              null);
      GameTestHelpers.assertTrue(
          helper, "The default preset " + presetLocation + " must spawn", importResult.success());

      EasyNPC<?> easyNPC =
          LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
      GameTestHelpers.assertNotNull(
          helper, "The default preset " + presetLocation + " must spawn an NPC", easyNPC);
      GameTestHelpers.assertEquals(
          helper,
          "The default preset " + presetLocation + " must spawn its NPC type",
          entityData.getStringOr(PresetData.ID_TAG, ""),
          EntityType.getKey(easyNPC.getEntity().getType()).toString());
      assertPresetObjectivesAreActive(helper, presetLocation, entityData, easyNPC);
      spawnedNPCs.put(presetLocation, easyNPC);
    }

    helper.runAfterDelay(
        40,
        () -> {
          spawnedNPCs.forEach(
              (presetLocation, easyNPC) ->
                  GameTestHelpers.assertTrue(
                      helper,
                      "The NPC of the default preset " + presetLocation + " must keep running",
                      easyNPC.getEntity().isAlive()));
          spawnedNPCs.values().forEach(easyNPC -> easyNPC.getEntity().discard());
          helper.succeed();
        });
  }

  private static void assertPresetObjectivesAreActive(
      GameTestHelper helper,
      Identifier presetLocation,
      CompoundTag entityData,
      EasyNPC<?> easyNPC) {
    ListTag presetObjectives =
        entityData
            .getCompoundOrEmpty(ObjectiveDataCapable.DATA_OBJECTIVE_DATA_TAG)
            .getListOrEmpty(ObjectiveDataSet.DATA_OBJECTIVE_DATA_SET_TAG);
    ObjectiveDataSet activeObjectives = easyNPC.getEasyNPCObjectiveData().getObjectiveDataSet();
    for (int index = 0; index < presetObjectives.size(); index++) {
      ObjectiveType objectiveType =
          new ObjectiveDataEntry(presetObjectives.getCompoundOrEmpty(index)).getType();
      GameTestHelpers.assertTrue(
          helper,
          "The default preset " + presetLocation + " must activate the objective " + objectiveType,
          activeObjectives.hasObjective(objectiveType));
    }
  }

  private static CompoundTag entityData(CompoundTag presetTag) {
    return PresetData.usesEntityDataWrapper(presetTag)
        ? presetTag.getCompoundOrEmpty(PresetData.DATA_TAG)
        : presetTag;
  }

  private static List<Identifier> defaultPresetLocations(GameTestHelper helper) {
    return DefaultPresetDataFiles.getPresetIdentifiers(helper.getLevel().getServer())
        .sorted(Comparator.comparing(Identifier::toString))
        .toList();
  }

  private static CompoundTag loadPreset(GameTestHelper helper, Identifier presetLocation) {
    return PresetHandler.loadPresetCompoundTag(
        PresetType.DEFAULT, presetLocation, helper.getLevel().getServer());
  }

  private static EasyNPC<?> importPreset(GameTestHelper helper, CompoundTag presetTag) {
    CompoundTag entityData = entityData(presetTag);
    EntityType<?> entityType =
        EntityType.byString(entityData.getStringOr(PresetData.ID_TAG, "")).orElse(null);
    if (entityType == null) {
      helper.fail("Unknown NPC type in " + entityData.getStringOr(PresetData.ID_TAG, ""));
      return null;
    }

    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();
    ((PresetDataCapable<?>) easyNPC).importPresetData(entityData.copy());
    return easyNPC;
  }

  private static CompoundTag withoutRegeneratedMetadata(CompoundTag entityData) {
    CompoundTag comparableData = entityData.copy();
    comparableData.remove(PresetDataCapable.PRESET_METADATA_TAG);
    return comparableData;
  }

  private static CompoundTag wrapAsPreset(CompoundTag originalPreset, CompoundTag entityData) {
    CompoundTag preset = new CompoundTag();
    if (originalPreset.contains(PresetDataCapable.PRESET_METADATA_TAG)) {
      preset.put(
          PresetDataCapable.PRESET_METADATA_TAG,
          originalPreset.getCompoundOrEmpty(PresetDataCapable.PRESET_METADATA_TAG));
    }

    CompoundTag sharedEntityData = entityData.copy();
    sharedEntityData.remove(PresetDataCapable.PRESET_METADATA_TAG);
    sharedEntityData.remove(NavigationDataCapable.DATA_NAVIGATION_TAG);
    sharedEntityData.remove(Entity.TAG_UUID);
    sharedEntityData.remove(PresetData.POSITION_TAG);
    sharedEntityData.remove(PresetData.ROTATION_TAG);
    preset.put(PresetData.DATA_TAG, sharedEntityData);
    return preset;
  }

  private static void writePreset(
      GameTestHelper helper, File targetFolder, Identifier presetLocation, CompoundTag presetTag) {
    String relativePath =
        presetLocation
            .getPath()
            .substring(DataFileHandler.RESOURCE_DEFAULT_PRESET_PATH.length() + 1);
    String snbtPath =
        PresetExportFormat.removePresetExtension(relativePath)
            + PresetExportFormat.SNBT.getFileExtension();

    File presetFile = new File(targetFolder, snbtPath);
    File parentFolder = presetFile.getParentFile();
    if (parentFolder != null && !parentFolder.exists() && !parentFolder.mkdirs()) {
      helper.fail("Unable to create the folder " + parentFolder);
      return;
    }

    PresetFileHandler.saveSnbt(presetFile, presetTag);
  }
}
