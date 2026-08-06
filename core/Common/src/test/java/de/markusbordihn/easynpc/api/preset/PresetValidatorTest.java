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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.action.MessageActionData;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.List;
import java.util.Set;
import net.minecraft.SharedConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class PresetValidatorTest {

  private static final String HUMANOID = "easy_npc:humanoid";

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static CompoundTag presetWithCustomAction(String command) {
    CompoundTag actionEntry = new CompoundTag();
    actionEntry.putString(ActionDataEntry.DATA_TYPE_TAG, ActionDataType.CUSTOM.name());
    actionEntry.putString(ActionDataEntry.DATA_COMMAND_TAG, command);
    return presetWithActionEntry(actionEntry);
  }

  private static CompoundTag presetWithActionCondition(CompoundTag conditionEntry) {
    ListTag conditionEntries = new ListTag();
    conditionEntries.add(conditionEntry);
    CompoundTag conditionDataSet = new CompoundTag();
    conditionDataSet.put(ConditionDataSet.CONDITION_DATA_SET_TAG, conditionEntries);

    CompoundTag actionEntry = new CompoundTag();
    actionEntry.putString(ActionDataEntry.DATA_TYPE_TAG, ActionDataType.CLOSE_DIALOG.name());
    actionEntry.put(ConditionDataSet.CONDITION_DATA_SET_TAG, conditionDataSet);
    return presetWithActionEntry(actionEntry);
  }

  private static CompoundTag presetWithActionEntry(CompoundTag actionEntry) {
    ListTag actionEntries = new ListTag();
    actionEntries.add(actionEntry);
    CompoundTag actionEventSet = new CompoundTag();
    actionEventSet.put("ON_INTERACTION", actionEntries);
    CompoundTag actionData = new CompoundTag();
    actionData.put(ActionEventSet.DATA_ACTION_EVENT_SET_TAG, actionEventSet);

    CompoundTag presetTag = new CompoundTag();
    presetTag.putString(PresetData.ID_TAG, HUMANOID);
    presetTag.put(ActionEventDataCapable.DATA_ACTION_DATA_TAG, actionData);
    return presetTag;
  }

  private static CompoundTag dialogEntryWithText(String label) {
    CompoundTag textEntry = new CompoundTag();
    textEntry.putString(DialogDataEntry.DATA_TEXT_TAG, "Hello");
    ListTag texts = new ListTag();
    texts.add(textEntry);

    CompoundTag dialogEntry = new CompoundTag();
    dialogEntry.putString(DialogDataEntry.DATA_LABEL_TAG, label);
    dialogEntry.put(DialogDataEntry.DATA_TEXTS_TAG, texts);
    return dialogEntry;
  }

  private static boolean hasRule(PresetValidationReport report, PresetValidationRule rule) {
    return report.issues().stream().anyMatch(issue -> issue.rule() == rule);
  }

  @Test
  @DisplayName("A minimal preset with its NPC type is valid")
  void testMinimalPresetIsValid() {
    CompoundTag presetTag = new CompoundTag();
    presetTag.putString(PresetData.ID_TAG, HUMANOID);

    assertTrue(PresetValidator.validate(presetTag).isValid());
  }

  @Test
  @DisplayName("An empty preset is rejected")
  void testEmptyPresetIsRejected() {
    assertFalse(PresetValidator.validate(new CompoundTag()).isValid());
    assertFalse(PresetValidator.validate(null).isValid());
  }

  @Test
  @DisplayName("A preset without its NPC type is rejected")
  void testPresetWithoutEntityTypeIsRejected() {
    CompoundTag presetTag = new CompoundTag();
    presetTag.putString("CustomName", "Trader");

    PresetValidationReport report = PresetValidator.validate(presetTag);

    assertFalse(report.isValid());
    assertTrue(hasRule(report, PresetValidationRule.MISSING_ENTITY_TYPE));
  }

  @Test
  @DisplayName("A preset with a malformed NPC type is rejected")
  void testMalformedEntityTypeIsRejected() {
    CompoundTag presetTag = new CompoundTag();
    presetTag.putString(PresetData.ID_TAG, "Not A Resource Location");

    assertTrue(
        hasRule(PresetValidator.validate(presetTag), PresetValidationRule.MALFORMED_ENTITY_TYPE));
  }

  @Test
  @DisplayName("An NPC type which is unknown here is only a warning")
  void testUnknownEntityTypeIsWarning() {
    CompoundTag presetTag = new CompoundTag();
    presetTag.putString(PresetData.ID_TAG, "other_mod:unknown");

    PresetValidationReport report =
        PresetValidator.validate(
            presetTag, PresetValidationContext.forKnownEntityTypes(Set.of(HUMANOID)));

    assertTrue(report.isValid());
    assertTrue(report.hasWarnings());
    assertTrue(hasRule(report, PresetValidationRule.UNKNOWN_ENTITY_TYPE));
  }

  @Test
  @DisplayName("A preset which stores its data in both shapes is rejected")
  void testMixedPresetShapeIsRejected() {
    CompoundTag entityData = new CompoundTag();
    entityData.putString(PresetData.ID_TAG, HUMANOID);
    CompoundTag presetTag = new CompoundTag();
    presetTag.put(PresetData.DATA_TAG, entityData);
    presetTag.putString(PresetData.ID_TAG, HUMANOID);

    assertTrue(
        hasRule(PresetValidator.validate(presetTag), PresetValidationRule.MIXED_PRESET_SHAPE));
  }

  @Test
  @DisplayName("A malformed parent reference is rejected")
  void testMalformedParentIsRejected() {
    CompoundTag presetTag = new CompoundTag();
    presetTag.putString(PresetData.ID_TAG, HUMANOID);
    presetTag.putString(PresetData.PARENT_TAG, "Not A Resource Location");

    assertFalse(PresetValidator.validate(presetTag).isValid());
  }

  @Test
  @DisplayName("A parent reference which is still present is reported")
  void testUnresolvedParentIsReported() {
    CompoundTag presetTag = new CompoundTag();
    presetTag.putString(PresetData.ID_TAG, HUMANOID);
    presetTag.putString(PresetData.PARENT_TAG, "easy_npc:api/preset/base/humanoid.npc.snbt");

    PresetValidationReport report = PresetValidator.validate(presetTag);

    assertTrue(report.isValid());
    assertTrue(hasRule(report, PresetValidationRule.UNRESOLVED_PARENT));
  }

  @Test
  @DisplayName("A dialog without text is rejected")
  void testDialogWithoutTextIsRejected() {
    CompoundTag dialogEntry = new CompoundTag();
    dialogEntry.putString(DialogDataEntry.DATA_LABEL_TAG, "welcome");
    ListTag dialogEntries = new ListTag();
    dialogEntries.add(dialogEntry);
    CompoundTag dialogData = new CompoundTag();
    dialogData.put(DialogDataSet.DATA_DIALOG_DATA_SET_TAG, dialogEntries);

    CompoundTag presetTag = new CompoundTag();
    presetTag.putString(PresetData.ID_TAG, HUMANOID);
    presetTag.put(DialogDataCapable.DATA_DIALOG_DATA_TAG, dialogData);

    assertTrue(
        hasRule(PresetValidator.validate(presetTag), PresetValidationRule.DIALOG_WITHOUT_TEXT));
  }

  @Test
  @DisplayName("A duplicated dialog label is rejected")
  void testDuplicatedDialogLabelIsRejected() {
    ListTag dialogEntries = new ListTag();
    dialogEntries.add(dialogEntryWithText("welcome"));
    dialogEntries.add(dialogEntryWithText("welcome"));
    CompoundTag dialogData = new CompoundTag();
    dialogData.put(DialogDataSet.DATA_DIALOG_DATA_SET_TAG, dialogEntries);

    CompoundTag presetTag = new CompoundTag();
    presetTag.putString(PresetData.ID_TAG, HUMANOID);
    presetTag.put(DialogDataCapable.DATA_DIALOG_DATA_TAG, dialogData);

    assertTrue(
        hasRule(PresetValidator.validate(presetTag), PresetValidationRule.DUPLICATE_DIALOG_LABEL));
  }

  @Test
  @DisplayName("An unknown action type is rejected")
  void testUnknownActionTypeIsRejected() {
    CompoundTag actionEntry = new CompoundTag();
    actionEntry.putString(ActionDataEntry.DATA_TYPE_TAG, "NOT_AN_ACTION");
    ListTag actionEntries = new ListTag();
    actionEntries.add(actionEntry);
    CompoundTag actionEventSet = new CompoundTag();
    actionEventSet.put("ON_INTERACTION", actionEntries);
    CompoundTag actionData = new CompoundTag();
    actionData.put(ActionEventSet.DATA_ACTION_EVENT_SET_TAG, actionEventSet);

    CompoundTag presetTag = new CompoundTag();
    presetTag.putString(PresetData.ID_TAG, HUMANOID);
    presetTag.put(ActionEventDataCapable.DATA_ACTION_DATA_TAG, actionData);

    assertTrue(
        hasRule(PresetValidator.validate(presetTag), PresetValidationRule.UNKNOWN_ACTION_TYPE));
  }

  @Test
  @DisplayName("A message action without a text list is rejected")
  void testMessageWithoutTextsIsRejected() {
    CompoundTag actionEntry = new CompoundTag();
    actionEntry.putString(ActionDataEntry.DATA_TYPE_TAG, ActionDataType.MESSAGE.name());
    actionEntry.putString(ActionDataEntry.DATA_COMMAND_TAG, "Legacy text");

    assertTrue(
        hasRule(
            PresetValidator.validate(presetWithActionEntry(actionEntry)),
            PresetValidationRule.MESSAGE_ACTION_WITHOUT_TEXT));
  }

  @Test
  @DisplayName("A message action with a text list is valid")
  void testMessageWithTextsIsValid() {
    CompoundTag actionEntry = new CompoundTag();
    actionEntry.putString(ActionDataEntry.DATA_TYPE_TAG, ActionDataType.MESSAGE.name());
    actionEntry.put(
        ActionDataEntry.DATA_MESSAGE_TAG,
        MessageActionData.DEFAULT.withTexts(List.of("Hello", "Welcome")).createTag());

    assertTrue(PresetValidator.validate(presetWithActionEntry(actionEntry)).isValid());
  }

  @Test
  @DisplayName("Unreadable SNBT is rejected with a readable message")
  void testMalformedSnbtIsRejected() {
    PresetValidationReport report = PresetValidator.validateSnbt("{id:\"easy_npc:humanoid\"");

    assertFalse(report.isValid());
    assertTrue(hasRule(report, PresetValidationRule.MALFORMED_SNBT));
  }

  @Test
  @DisplayName("An empty preset file is rejected")
  void testEmptySnbtIsRejected() {
    assertFalse(PresetValidator.validateSnbt("   ").isValid());
  }

  @Test
  @DisplayName("Identity data is only reported for a shared preset")
  void testIdentityDataIsReportedForSharedPresets() {
    CompoundTag presetTag = new CompoundTag();
    presetTag.putString(PresetData.ID_TAG, HUMANOID);
    CompoundTagUtils.writeUUID(presetTag, PresetData.UUID_TAG, java.util.UUID.randomUUID());

    assertFalse(
        hasRule(PresetValidator.validate(presetTag), PresetValidationRule.IDENTITY_DATA_PRESENT));
    assertTrue(
        hasRule(
            PresetValidator.validate(
                presetTag, PresetValidationContext.offline().withIdentityFreePreset()),
            PresetValidationRule.IDENTITY_DATA_PRESENT));
  }

  @Test
  @DisplayName("A dialog label longer than the limit is rejected")
  void testTooLongDialogLabelIsRejected() {
    ListTag dialogEntries = new ListTag();
    dialogEntries.add(dialogEntryWithText("A".repeat(DialogDataEntry.MAX_DIALOG_LABEL_LENGTH + 1)));
    CompoundTag dialogData = new CompoundTag();
    dialogData.put(DialogDataSet.DATA_DIALOG_DATA_SET_TAG, dialogEntries);

    CompoundTag presetTag = new CompoundTag();
    presetTag.putString(PresetData.ID_TAG, HUMANOID);
    presetTag.put(DialogDataCapable.DATA_DIALOG_DATA_TAG, dialogData);

    assertTrue(
        hasRule(PresetValidator.validate(presetTag), PresetValidationRule.DIALOG_LABEL_TOO_LONG));
  }

  @Test
  @DisplayName("An unknown condition type is rejected")
  void testUnknownConditionTypeIsRejected() {
    CompoundTag conditionEntry = new CompoundTag();
    conditionEntry.putString(ConditionDataEntry.DATA_TYPE_TAG, "NOT_A_CONDITION");

    assertTrue(
        hasRule(
            PresetValidator.validate(presetWithActionCondition(conditionEntry)),
            PresetValidationRule.UNKNOWN_CONDITION_TYPE));
  }

  @Test
  @DisplayName("A malformed custom condition id is rejected")
  void testMalformedCustomConditionIdIsRejected() {
    CompoundTag conditionEntry = new CompoundTag();
    conditionEntry.putString(ConditionDataEntry.DATA_TYPE_TAG, ConditionType.CUSTOM.name());
    conditionEntry.putString(
        ConditionDataEntry.DATA_CUSTOM_CONDITION_ID_TAG, "Not A Resource Location");

    assertTrue(
        hasRule(
            PresetValidator.validate(presetWithActionCondition(conditionEntry)),
            PresetValidationRule.MALFORMED_CUSTOM_CONDITION_ID));
  }

  @Test
  @DisplayName("An unregistered custom condition is only reported with a live registry")
  void testUnknownCustomConditionIdIsWarning() {
    CompoundTag conditionEntry = new CompoundTag();
    conditionEntry.putString(ConditionDataEntry.DATA_TYPE_TAG, ConditionType.CUSTOM.name());
    conditionEntry.putString(ConditionDataEntry.DATA_CUSTOM_CONDITION_ID_TAG, "my_mod:has_quest");
    CompoundTag presetTag = presetWithActionCondition(conditionEntry);

    assertFalse(
        hasRule(
            PresetValidator.validate(presetTag), PresetValidationRule.UNKNOWN_CUSTOM_CONDITION_ID));

    PresetValidationReport report =
        PresetValidator.validate(presetTag, PresetValidationContext.forServer(Set.of(HUMANOID)));

    assertTrue(report.isValid());
    assertTrue(hasRule(report, PresetValidationRule.UNKNOWN_CUSTOM_CONDITION_ID));
  }

  @Test
  @DisplayName("A malformed custom action id is rejected")
  void testMalformedCustomActionIdIsRejected() {
    assertTrue(
        hasRule(
            PresetValidator.validate(presetWithCustomAction("Not A Resource Location")),
            PresetValidationRule.MALFORMED_CUSTOM_ACTION_ID));
  }

  @Test
  @DisplayName("An unregistered custom action is only reported with a live registry")
  void testUnknownCustomActionIdIsWarning() {
    CompoundTag presetTag = presetWithCustomAction("my_mod:teleport home");

    assertFalse(
        hasRule(
            PresetValidator.validate(presetTag), PresetValidationRule.UNKNOWN_CUSTOM_ACTION_ID));

    PresetValidationReport report =
        PresetValidator.validate(presetTag, PresetValidationContext.forServer(Set.of(HUMANOID)));

    assertTrue(report.isValid());
    assertTrue(hasRule(report, PresetValidationRule.UNKNOWN_CUSTOM_ACTION_ID));
  }
}
