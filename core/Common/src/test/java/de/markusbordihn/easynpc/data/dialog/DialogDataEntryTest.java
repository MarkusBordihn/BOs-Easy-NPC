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

import java.util.LinkedHashSet;
import java.util.Set;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("DialogDataEntry Tests")
class DialogDataEntryTest {

  @Test
  @DisplayName("Should generate consistent hashCode for identical data")
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
  @DisplayName("Should preserve data through encode/decode cycle")
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
  @DisplayName("Should generate same UUID after encode/decode cycle")
  void testUUIDConsistencyAfterEncodeDecode() {
    DialogDataEntry original = new DialogDataEntry("dialog_label", "Dialog Name", "Dialog Text");
    CompoundTag tag = new CompoundTag();
    original.save(tag);
    DialogDataEntry decoded = new DialogDataEntry(tag);

    assertEquals(original.getId(), decoded.getId());
  }

  @Test
  @DisplayName("Should preserve label when changed and re-encoded")
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
  @DisplayName("Should handle dialog buttons correctly")
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
  @DisplayName("Should generate different UUIDs for different labels")
  void testDifferentUUIDsForDifferentLabels() {
    DialogDataEntry entry1 = new DialogDataEntry("label1", "Name", "Text");
    DialogDataEntry entry2 = new DialogDataEntry("label2", "Name", "Text");

    assertNotEquals(entry1.getId(), entry2.getId());
  }

  @Test
  @DisplayName("Should handle empty label correctly")
  void testEmptyLabel() {
    DialogDataEntry entry = new DialogDataEntry("", "TestName", "Text");

    assertNotNull(entry.getLabel());
    assertFalse(entry.getLabel().isEmpty());
    assertNotNull(entry.getId());
  }

  @Test
  @DisplayName("Should handle null label correctly")
  void testNullLabel() {
    DialogDataEntry entry = new DialogDataEntry(null, "TestName", "Text");

    assertNotNull(entry.getLabel());
    assertFalse(entry.getLabel().isEmpty());
    assertNotNull(entry.getId());
  }

  @Test
  @DisplayName("Should preserve priority through encode/decode")
  void testPriorityPreservation() {
    DialogDataEntry original = new DialogDataEntry("label", "Name", "Text");
    CompoundTag tag = new CompoundTag();
    original.save(tag);
    DialogDataEntry decoded = new DialogDataEntry(tag);

    assertEquals(original.getPriority(), decoded.getPriority());
    assertEquals(original.getId(), decoded.getId());
  }

  @Test
  @DisplayName("Should handle special characters in label")
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
  @DisplayName("Should ensure label consistency determines UUID consistency")
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
}
