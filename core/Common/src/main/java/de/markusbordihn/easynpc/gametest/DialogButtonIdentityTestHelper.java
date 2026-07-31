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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogButtonType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import java.util.LinkedHashSet;
import java.util.Set;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class DialogButtonIdentityTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 SECOND_NPC_POSITION = new Vec3(2, 2, 2);

  private DialogButtonIdentityTestHelper() {}

  public static void assertDuplicateButtonLabelIsRejected(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    DialogDataCapable<?> dialogData = easyNPC.getEasyNPCDialogData();
    GameTestHelpers.assertNotNull(helper, "NPC must support dialogs", dialogData);

    DialogButtonEntry firstButton = commandButton("first_button", "/say first");
    DialogButtonEntry secondButton = commandButton("second_button", "/say second");
    DialogDataSet dialogDataSet = dialogDataSet(firstButton, secondButton);
    dialogData.setDialogDataSet(dialogDataSet);

    DialogDataEntry dialogDataEntry = dialogDataSet.getDialog("test_dialog");
    DialogButtonEntry hijackingButton = secondButton.withLabel(firstButton.label());
    dialogDataEntry.setDialogButton(secondButton.id(), hijackingButton);

    GameTestHelpers.assertEquals(
        helper, "Both dialog buttons must survive", 2, dialogDataEntry.getNumberOfDialogButtons());
    GameTestHelpers.assertEquals(
        helper,
        "The first button must keep its own action",
        "/say first",
        firstCommandOf(dialogDataEntry.getDialogButton(firstButton.id())));
    GameTestHelpers.assertEquals(
        helper,
        "The second button must keep its own action",
        "/say second",
        firstCommandOf(dialogDataEntry.getDialogButton(secondButton.id())));
  }

  public static void assertButtonIdentitySurvivesSaveAndLoad(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    DialogDataCapable<?> dialogData = easyNPC.getEasyNPCDialogData();
    GameTestHelpers.assertNotNull(helper, "NPC must support dialogs", dialogData);

    DialogButtonEntry firstButton = commandButton("first_button", "/say first");
    DialogButtonEntry secondButton = commandButton("second_button", "/say second");
    dialogData.setDialogDataSet(dialogDataSet(firstButton, secondButton));

    EasyNPC<?> reloadedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SECOND_NPC_POSITION);
    DialogDataCapable<?> reloadedDialogData = reloadedNPC.getEasyNPCDialogData();
    reloadedDialogData.setDialogDataSet(
        new DialogDataSet(dialogData.getDialogDataSet().createTag()));

    DialogDataEntry reloadedDialog = reloadedDialogData.getDialogDataSet().getDialog("test_dialog");
    GameTestHelpers.assertNotNull(helper, "The dialog must survive save and load", reloadedDialog);
    GameTestHelpers.assertEquals(
        helper,
        "Both dialog buttons must survive save and load",
        2,
        reloadedDialog.getNumberOfDialogButtons());
    GameTestHelpers.assertEquals(
        helper,
        "The first button must keep its id and action",
        "/say first",
        firstCommandOf(reloadedDialog.getDialogButton(firstButton.id())));
    GameTestHelpers.assertEquals(
        helper,
        "The second button must keep its id and action",
        "/say second",
        firstCommandOf(reloadedDialog.getDialogButton(secondButton.id())));
  }

  public static void assertEditedButtonLabelStaysAddressable(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    DialogDataCapable<?> dialogData = easyNPC.getEasyNPCDialogData();
    GameTestHelpers.assertNotNull(helper, "NPC must support dialogs", dialogData);

    DialogButtonEntry button = commandButton("first_button", "/say first");
    DialogDataSet dialogDataSet = dialogDataSet(button);
    dialogData.setDialogDataSet(dialogDataSet);

    DialogDataEntry dialogDataEntry = dialogDataSet.getDialog("test_dialog");
    DialogButtonEntry renamedButton = button.withLabel("Yes, Please!");
    dialogDataEntry.setDialogButton(button.id(), renamedButton);

    GameTestHelpers.assertEquals(
        helper, "An edited button label is normalized", "yes_please", renamedButton.label());

    DialogDataSet reloadedDialogDataSet = new DialogDataSet(dialogDataSet.createTag());
    DialogDataEntry reloadedDialog = reloadedDialogDataSet.getDialog("test_dialog");
    GameTestHelpers.assertEquals(
        helper,
        "The renamed button stays addressable after save and load",
        "/say first",
        firstCommandOf(reloadedDialog.getDialogButton(renamedButton.id())));
  }

  private static String firstCommandOf(DialogButtonEntry dialogButtonEntry) {
    if (dialogButtonEntry == null || dialogButtonEntry.actionDataSet().isEmpty()) {
      return null;
    }

    return dialogButtonEntry.actionDataSet().getEntries().iterator().next().command();
  }

  private static DialogButtonEntry commandButton(String label, String command) {
    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(new ActionDataEntry(ActionDataType.COMMAND, null, command));
    return new DialogButtonEntry(label, label, DialogButtonType.DEFAULT, actionDataSet);
  }

  private static DialogDataSet dialogDataSet(DialogButtonEntry... dialogButtonEntries) {
    Set<DialogButtonEntry> dialogButtons = new LinkedHashSet<>();
    for (DialogButtonEntry dialogButtonEntry : dialogButtonEntries) {
      dialogButtons.add(dialogButtonEntry);
    }

    DialogDataEntry dialogDataEntry =
        new DialogDataEntry("test_dialog", "Test Dialog", "Text", dialogButtons);
    DialogDataSet dialogDataSet = new DialogDataSet();
    dialogDataSet.setDialog(dialogDataEntry.getId(), dialogDataEntry);
    return dialogDataSet;
  }
}
