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

package de.markusbordihn.easynpc.data.dialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class DialogLabelTest {

  @Test
  @DisplayName("An already normalized label is taken over unchanged")
  void normalizedLabelIsKept() {
    assertEquals(
        "aerial_manipulator", DialogUtils.generateButtonLabel("aerial_manipulator", "Any"));
  }

  @Test
  @DisplayName("An authored label that cannot be used is normalized")
  void authoredLabelIsNormalized() {
    assertEquals(
        "story_beat_one", DialogUtils.generateButtonLabel("Story.Beat One", "Fallback Name"));
  }

  @Test
  @DisplayName("Without an authored label the name is used as the fallback")
  void nameIsUsedAsFallback() {
    assertEquals("fallback_name", DialogUtils.generateButtonLabel("", "Fallback Name"));
    assertEquals("fallback_name", DialogUtils.generateButtonLabel(null, "Fallback Name"));
  }

  @Test
  @DisplayName("A readable name next to a valid label leaves the label untouched")
  void readableNameDoesNotChangeTheLabel() {
    DialogDataEntry dialogDataEntry =
        new DialogDataEntry("damaged_core", "Observatory Arrival Damaged Core", "Text");

    assertEquals("damaged_core", dialogDataEntry.getLabel());
  }

  @Test
  @DisplayName("A dialog saved without its generated label loads it back without a warning")
  void generatedLabelLoadsWithoutWarning() {
    DialogDataEntry savedDialog = new DialogDataEntry(null, "Round Trip Name", "Text", null);
    CompoundTag savedTag = savedDialog.save(new CompoundTag());

    DialogDataEntry loadedDialog = new DialogDataEntry(savedTag);

    assertFalse(savedTag.contains(DialogDataEntry.DATA_LABEL_TAG));
    assertEquals("round_trip_name", loadedDialog.getLabel());
    assertEquals(savedDialog.getId(), loadedDialog.getId());
    assertFalse(DialogUtils.reportedLabelChanges.contains("Round Trip Name"));
  }
}
