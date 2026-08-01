/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.data.dialog;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.condition.DurationType;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class DialogDataEntryTest {

  @Test
  void testConsistentHashCode() {
    DialogDataEntry entry1 = new DialogDataEntry("test_label", "Test Name", "Test Text");
    DialogDataEntry entry2 = new DialogDataEntry("test_label", "Test Name", "Test Text");

    assertEquals(entry1.getId(), entry2.getId());
  }

  @Test
  @DisplayName("Should generate UUID based on label")
  void testUUIDGenerationFromLabel() {
    DialogDataEntry entry1 = new DialogDataEntry("unique_label", "Name 1", "Text 1");
    DialogDataEntry entry2 = new DialogDataEntry("unique_label", "Name 2", "Text 2");

    assertEquals(entry1.getId(), entry2.getId());
  }

  @Test
  void testEncodeDecodeConsistency() {
    DialogDataEntry original = new DialogDataEntry("test_dialog", "Test Dialog", "Test text");
    CompoundTag tag = new CompoundTag();
    original.save(tag);
    DialogDataEntry decoded = new DialogDataEntry(tag);

    assertEquals(original.getLabel(), decoded.getLabel());
    assertEquals(original.getName(), decoded.getName());
    assertEquals(original.getText(), decoded.getText());
    assertEquals(original.getId(), decoded.getId());
  }

  @Test
  void testUUIDConsistencyAfterEncodeDecode() {
    DialogDataEntry original = new DialogDataEntry("dialog_label", "Dialog Name", "Dialog Text");
    CompoundTag tag = new CompoundTag();
    original.save(tag);
    DialogDataEntry decoded = new DialogDataEntry(tag);

    assertEquals(original.getId(), decoded.getId());
  }

  @Test
  void testLabelChangePreservation() {
    DialogDataEntry original = new DialogDataEntry("original_label", "Name", "Text");
    UUID originalId = original.getId();
    original.setLabel("new_label");

    assertNotEquals(originalId, original.getId());

    CompoundTag tag = new CompoundTag();
    original.save(tag);
    DialogDataEntry decoded = new DialogDataEntry(tag);

    assertEquals(original.getId(), decoded.getId());
    assertEquals("new_label", decoded.getLabel());
  }

  @Test
  void testDialogButtonsEncodeDecode() {
    Set<DialogButtonEntry> buttons = new LinkedHashSet<>();
    buttons.add(new DialogButtonEntry("Button 1", DialogButtonType.DEFAULT));
    buttons.add(new DialogButtonEntry("Button 2", DialogButtonType.DEFAULT));
    DialogDataEntry original = new DialogDataEntry("label", "Name", "Text", buttons);

    CompoundTag tag = new CompoundTag();
    original.save(tag);
    DialogDataEntry decoded = new DialogDataEntry(tag);

    assertEquals(original.getDialogButtons().size(), decoded.getDialogButtons().size());
    assertEquals(original.getId(), decoded.getId());
  }

  @Test
  void testDifferentUUIDsForDifferentLabels() {
    DialogDataEntry entry1 = new DialogDataEntry("label1", "Name", "Text");
    DialogDataEntry entry2 = new DialogDataEntry("label2", "Name", "Text");

    assertNotEquals(entry1.getId(), entry2.getId());
  }

  @Test
  void testEmptyLabel() {
    DialogDataEntry entry = new DialogDataEntry("", "TestName", "Text");

    assertNotNull(entry.getLabel());
    assertFalse(entry.getLabel().isEmpty());
    assertNotNull(entry.getId());
  }

  @Test
  void testNullLabel() {
    DialogDataEntry entry = new DialogDataEntry(null, "TestName", "Text");

    assertNotNull(entry.getLabel());
    assertFalse(entry.getLabel().isEmpty());
    assertNotNull(entry.getId());
  }

  @Test
  void testPriorityPreservation() {
    DialogDataEntry original = new DialogDataEntry("label", "Name", "Text");
    CompoundTag tag = new CompoundTag();
    original.save(tag);
    DialogDataEntry decoded = new DialogDataEntry(tag);

    assertEquals(original.getPriority(), decoded.getPriority());
    assertEquals(original.getId(), decoded.getId());
  }

  @Test
  void testSpecialCharactersInLabel() {
    DialogDataEntry entry1 = new DialogDataEntry("test_label_with_üöä_and_!@#", "Name", "Text");
    DialogDataEntry entry2 = new DialogDataEntry("test_label_with_üöä_and_!@#", "Name", "Text");

    assertEquals(entry1.getId(), entry2.getId());

    CompoundTag tag = new CompoundTag();
    entry1.save(tag);
    DialogDataEntry decoded = new DialogDataEntry(tag);

    assertEquals(entry1.getId(), decoded.getId());
  }

  @Test
  void testLabelConsistencyDeterminesUUIDConsistency() {
    DialogDataEntry clientEntry =
        new DialogDataEntry("critical_test_label", "Client Name", "Client Text");
    DialogDataEntry serverEntry =
        new DialogDataEntry("critical_test_label", "Server Name", "Server Text");

    assertEquals(clientEntry.getId(), serverEntry.getId());

    CompoundTag clientTag = new CompoundTag();
    clientEntry.save(clientTag);
    DialogDataEntry clientDecoded = new DialogDataEntry(clientTag);

    CompoundTag serverTag = new CompoundTag();
    serverEntry.save(serverTag);
    DialogDataEntry serverDecoded = new DialogDataEntry(serverTag);

    assertEquals(clientDecoded.getId(), serverDecoded.getId());
  }

  @Test
  void testLegacyExecutionLimitConditionLoadsFromDialog() {
    CompoundTag dialogTag = new CompoundTag();
    dialogTag.putString(DialogDataEntry.DATA_DIALOG_NAME, "Legacy Dialog");

    ListTag texts = new ListTag();
    CompoundTag textTag = new CompoundTag();
    textTag.putString(DialogDataEntry.DATA_TEXT_TAG, "Legacy text");
    texts.add(textTag);
    dialogTag.put(DialogDataEntry.DATA_TEXTS_TAG, texts);

    ListTag conditions = new ListTag();
    CompoundTag conditionTag = new CompoundTag();
    conditionTag.putString(ConditionDataEntry.DATA_TYPE_TAG, "EXECUTION_LIMIT");
    conditionTag.putString(ConditionDataEntry.DATA_LEGACY_TEXT_TAG, "PER_DAY");
    conditionTag.putInt(ConditionDataEntry.DATA_VALUE_TAG, 1);
    conditions.add(conditionTag);
    dialogTag.put(DialogDataEntry.DATA_CONDITIONS_TAG, conditions);

    DialogDataEntry entry = new DialogDataEntry(dialogTag);

    assertTrue(entry.hasConditions());
    assertEquals(1, entry.getConditions().size());
    ConditionDataEntry loadedCondition = entry.getConditions().iterator().next();
    assertEquals(ConditionType.EXECUTION_LIMIT, loadedCondition.conditionType());
    assertEquals(DurationType.PER_DAY, loadedCondition.subType());
    assertEquals(1, loadedCondition.value());
  }

  @Test
  @DisplayName("Name and text keep every character while the label is converted")
  void testSpecialCharactersInNameAndText() {
    DialogDataEntry entry =
        new DialogDataEntry(null, "Händler Begrüßung", "Willkommen, mein Freund! 🐢");

    assertEquals("Händler Begrüßung", entry.getName());
    assertEquals("Willkommen, mein Freund! 🐢", entry.getText());
    assertEquals("haendler_begruessung", entry.getLabel());
  }

  @Test
  @DisplayName("Dialogs with non latin names get their own label and id")
  void testNonLatinNamesGetSeparateIds() {
    DialogDataEntry cyrillic = new DialogDataEntry(null, "Модель", "Текст");
    DialogDataEntry japanese = new DialogDataEntry(null, "モデル", "テキスト");

    assertFalse(cyrillic.getLabel().isEmpty());
    assertFalse(japanese.getLabel().isEmpty());
    assertNotEquals(cyrillic.getLabel(), japanese.getLabel());
    assertNotEquals(cyrillic.getId(), japanese.getId());
    assertEquals("Модель", cyrillic.getName());
  }

  @Test
  @DisplayName("Stored labels and ids survive a reload unchanged")
  void testStoredLabelsStayStable() {
    DialogDataEntry original = new DialogDataEntry("hndler_begrung", "Händler", "Hallo");
    CompoundTag tag = new CompoundTag();
    original.save(tag);
    DialogDataEntry reloaded = new DialogDataEntry(tag);

    assertEquals("hndler_begrung", reloaded.getLabel());
    assertEquals(original.getId(), reloaded.getId());
  }
}
