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

package de.markusbordihn.easynpc.api.preset;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import java.util.ArrayList;
import java.util.List;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.TagParser;
import net.minecraft.resources.Identifier;

public class PresetValidator {

  private static final List<String> IDENTITY_TAGS =
      List.of(PresetData.UUID_TAG, "Pos", "Rotation", OwnerDataCapable.DATA_OWNER_TAG);
  private static final String ROOT_PATH = "";

  private PresetValidator() {}

  public static PresetValidationReport validate(CompoundTag presetTag) {
    return validate(presetTag, PresetValidationContext.offline());
  }

  public static PresetValidationReport validate(
      CompoundTag presetTag, PresetValidationContext context) {
    List<PresetValidationIssue> issues = new ArrayList<>();

    if (presetTag == null || presetTag.isEmpty()) {
      issues.add(
          PresetValidationIssue.error(
              PresetValidationRule.EMPTY_PRESET, ROOT_PATH, "The preset has no content"));
      return new PresetValidationReport(issues);
    }

    validateParent(presetTag, issues);

    boolean usesWrapper = PresetData.usesEntityDataWrapper(presetTag);
    if (presetTag.contains(PresetData.DATA_TAG) && presetTag.contains(PresetData.ID_TAG)) {
      issues.add(
          PresetValidationIssue.error(
              PresetValidationRule.MIXED_PRESET_SHAPE,
              ROOT_PATH,
              "The preset stores its entity data in a '"
                  + PresetData.DATA_TAG
                  + "' tag and at the top level at the same time"));
    }

    String entityDataPath = usesWrapper ? PresetData.DATA_TAG : ROOT_PATH;
    CompoundTag entityData =
        usesWrapper ? presetTag.getCompoundOrEmpty(PresetData.DATA_TAG) : presetTag;

    validateEntityType(entityData, entityDataPath, context, issues);
    PresetDialogValidator.validateDialogData(entityData, entityDataPath, context, issues);
    PresetActionValidator.validateActionData(entityData, entityDataPath, context, issues);
    PresetObjectiveValidator.validateObjectiveData(entityData, entityDataPath, issues);
    validateDataVersion(entityData, entityDataPath, issues);

    if (context.expectsIdentityFreePreset()) {
      validateIdentityData(entityData, entityDataPath, issues);
    }

    return new PresetValidationReport(issues);
  }

  public static PresetValidationReport validateSnbt(String presetContent) {
    return validateSnbt(presetContent, PresetValidationContext.offline());
  }

  public static PresetValidationReport validateSnbt(
      String presetContent, PresetValidationContext context) {
    if (presetContent == null || presetContent.isBlank()) {
      return new PresetValidationReport(
          List.of(
              PresetValidationIssue.error(
                  PresetValidationRule.EMPTY_PRESET, ROOT_PATH, "The preset file is empty")));
    }

    try {
      return validate(TagParser.parseCompoundFully(presetContent), context);
    } catch (CommandSyntaxException exception) {
      return new PresetValidationReport(
          List.of(
              PresetValidationIssue.error(
                  PresetValidationRule.MALFORMED_SNBT,
                  ROOT_PATH,
                  "The preset is not readable: " + exception.getMessage())));
    }
  }

  private static void validateParent(CompoundTag presetTag, List<PresetValidationIssue> issues) {
    if (!presetTag.contains(PresetData.PARENT_TAG)) {
      return;
    }

    String parent = presetTag.getStringOr(PresetData.PARENT_TAG, "");
    if (Identifier.tryParse(parent) == null) {
      issues.add(
          PresetValidationIssue.error(
              PresetValidationRule.MALFORMED_PARENT,
              PresetData.PARENT_TAG,
              "The parent preset id '" + parent + "' is not a valid resource location"));
      return;
    }

    issues.add(
        PresetValidationIssue.info(
            PresetValidationRule.UNRESOLVED_PARENT,
            PresetData.PARENT_TAG,
            "The preset builds on " + parent + " and has to be resolved before it is used"));
  }

  private static void validateEntityType(
      CompoundTag entityData,
      String path,
      PresetValidationContext context,
      List<PresetValidationIssue> issues) {
    if (!entityData.contains(PresetData.ID_TAG)) {
      issues.add(
          PresetValidationIssue.error(
              PresetValidationRule.MISSING_ENTITY_TYPE,
              PresetValidationSupport.childPath(path, PresetData.ID_TAG),
              "The preset does not name its NPC type"));
      return;
    }

    String entityTypeId = entityData.getStringOr(PresetData.ID_TAG, "");
    if (Identifier.tryParse(entityTypeId) == null) {
      issues.add(
          PresetValidationIssue.error(
              PresetValidationRule.MALFORMED_ENTITY_TYPE,
              PresetValidationSupport.childPath(path, PresetData.ID_TAG),
              "The NPC type '" + entityTypeId + "' is not a valid resource location"));
      return;
    }

    if (!context.knownEntityTypes().test(entityTypeId)) {
      issues.add(
          PresetValidationIssue.warning(
              PresetValidationRule.UNKNOWN_ENTITY_TYPE,
              PresetValidationSupport.childPath(path, PresetData.ID_TAG),
              "The NPC type " + entityTypeId + " is not available here"));
    }
  }

  private static void validateDataVersion(
      CompoundTag entityData, String path, List<PresetValidationIssue> issues) {
    if (!entityData.contains(ConfigDataCapable.DATA_EASY_NPC_DATA_VERSION_TAG)) {
      return;
    }

    int dataVersion = entityData.getIntOr(ConfigDataCapable.DATA_EASY_NPC_DATA_VERSION_TAG, 0);
    if (dataVersion < Constants.NPC_DATA_VERSION) {
      issues.add(
          PresetValidationIssue.info(
              PresetValidationRule.OUTDATED_DATA_VERSION,
              PresetValidationSupport.childPath(
                  path, ConfigDataCapable.DATA_EASY_NPC_DATA_VERSION_TAG),
              "The preset was written for data version "
                  + dataVersion
                  + " instead of "
                  + Constants.NPC_DATA_VERSION));
    }
  }

  private static void validateIdentityData(
      CompoundTag entityData, String path, List<PresetValidationIssue> issues) {
    for (String identityTag : IDENTITY_TAGS) {
      if (entityData.contains(identityTag)) {
        issues.add(
            PresetValidationIssue.warning(
                PresetValidationRule.IDENTITY_DATA_PRESENT,
                PresetValidationSupport.childPath(path, identityTag),
                "A shared preset should not carry " + identityTag));
      }
    }
  }
}
