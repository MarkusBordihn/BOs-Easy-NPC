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

package de.markusbordihn.easynpc.data.dialog;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class DialogButtonEntryTest {

  @Test
  @DisplayName("An edited label is normalized before the id is derived from it")
  void testLabelIsNormalizedOnEdit() {
    DialogButtonEntry entry =
        new DialogButtonEntry("Yes please", DialogButtonType.DEFAULT).withLabel("Yes, Please!");

    assertEquals("yes_please", entry.label());
    assertEquals(
        new DialogButtonEntry("Yes please", DialogButtonType.DEFAULT).withLabel("yes_please").id(),
        entry.id());
  }

  @Test
  @DisplayName("An empty label falls back to the generated label of the button name")
  void testEmptyLabelFallsBackToName() {
    DialogButtonEntry entry =
        new DialogButtonEntry("Ask for help", DialogButtonType.DEFAULT).withLabel("");

    assertEquals(DialogUtils.generateButtonLabel("Ask for help"), entry.label());
  }

  @Test
  @DisplayName("A button label already used by another button is rejected")
  void testDuplicateLabelIsRejected() {
    DialogDataEntry dialogDataEntry = new DialogDataEntry("question", "Question", "Text");
    DialogButtonEntry firstButton = new DialogButtonEntry("Button 1", DialogButtonType.DEFAULT);
    DialogButtonEntry secondButton = new DialogButtonEntry("Button 2", DialogButtonType.DEFAULT);
    dialogDataEntry.setDialogButton(firstButton);
    dialogDataEntry.setDialogButton(secondButton);

    DialogButtonEntry hijackedButton = secondButton.withLabel(firstButton.label());
    assertTrue(dialogDataEntry.hasConflictingDialogButton(secondButton.id(), hijackedButton));

    dialogDataEntry.setDialogButton(secondButton.id(), hijackedButton);
    assertEquals(2, dialogDataEntry.getNumberOfDialogButtons());
    assertEquals(secondButton, dialogDataEntry.getDialogButton(secondButton.id()));
  }

  @Test
  @DisplayName("Editing a button without touching its label still replaces it")
  void testEditKeepsSingleEntry() {
    DialogDataEntry dialogDataEntry = new DialogDataEntry("question", "Question", "Text");
    DialogButtonEntry button = new DialogButtonEntry("Button 1", DialogButtonType.DEFAULT);
    dialogDataEntry.setDialogButton(button);

    DialogButtonEntry renamedButton = button.withName("Renamed");
    assertFalse(dialogDataEntry.hasConflictingDialogButton(button.id(), renamedButton));

    dialogDataEntry.setDialogButton(button.id(), renamedButton);
    assertEquals(1, dialogDataEntry.getNumberOfDialogButtons());
    assertEquals("Renamed", dialogDataEntry.getDialogButton(button.id()).name());
  }

  @Test
  @DisplayName("A stored button keeps its id after the tag round-trip")
  void testIdSurvivesRoundTrip() {
    DialogButtonEntry entry =
        new DialogButtonEntry("Button 1", DialogButtonType.DEFAULT).withLabel("custom_label");

    assertEquals(entry.id(), new DialogButtonEntry(entry.createTag()).id());
  }
}
