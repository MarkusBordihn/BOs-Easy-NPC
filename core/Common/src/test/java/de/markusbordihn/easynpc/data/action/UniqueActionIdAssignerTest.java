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
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 * OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.data.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.trading.TradingDataSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.TagParser;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class UniqueActionIdAssignerTest {

  private static final String BANK_DIALOGS =
      "{Type:\"CUSTOM\",DialogDataSet:["
          + "{Label:\"deposit\",Texts:[{Text:\"Deposit\"}],Buttons:["
          + "{Name:\"Five\",Label:\"five\",Actions:[{Type:\"COMMAND\",Cmd:\"say bank\"}]},"
          + "{Name:\"Ten\",Label:\"ten\",Actions:[{Type:\"COMMAND\",Cmd:\"say other\"}]}]},"
          + "{Label:\"withdraw\",Texts:[{Text:\"Withdraw\"}],Buttons:["
          + "{Name:\"Five\",Label:\"five\",Actions:[{Type:\"COMMAND\",Cmd:\"say bank\"}]}]}]}";

  private static DialogDataSet loadDialogs() throws CommandSyntaxException {
    return new DialogDataSet(TagParser.parseTag(BANK_DIALOGS));
  }

  private static ActionDataSet loadActions(String... commands) {
    ListTag actionDataList = new ListTag();
    for (String command : commands) {
      CompoundTag actionTag = new CompoundTag();
      actionTag.putString(ActionDataEntry.DATA_TYPE_TAG, ActionDataType.COMMAND.name());
      actionTag.putString(ActionDataEntry.DATA_COMMAND_TAG, command);
      actionDataList.add(actionTag);
    }
    return new ActionDataSet().load(actionDataList);
  }

  private static UUID firstActionId(DialogDataSet dialogDataSet, String dialogLabel) {
    return dialogDataSet
        .getDialog(dialogLabel)
        .getDialogButton("five")
        .actionDataSet()
        .getOrderedEntries()
        .get(0)
        .id();
  }

  @Test
  @DisplayName("Equal actions in buttons of different dialogs get separate ids")
  void testEqualDialogButtonActionsGetSeparateIds() throws CommandSyntaxException {
    DialogDataSet dialogDataSet = loadDialogs();
    UUID derivedId = firstActionId(dialogDataSet, "deposit");
    assertEquals(derivedId, firstActionId(dialogDataSet, "withdraw"));

    UniqueActionIdAssigner.assign(dialogDataSet, null, null);

    assertEquals(derivedId, firstActionId(dialogDataSet, "deposit"));
    assertNotEquals(derivedId, firstActionId(dialogDataSet, "withdraw"));
  }

  @Test
  @DisplayName("Reassigned ids are the same on every load")
  void testReassignedIdsAreDeterministic() throws CommandSyntaxException {
    DialogDataSet firstLoad = loadDialogs();
    DialogDataSet secondLoad = loadDialogs();

    UniqueActionIdAssigner.assign(firstLoad, null, null);
    UniqueActionIdAssigner.assign(secondLoad, null, null);

    assertEquals(firstActionId(firstLoad, "withdraw"), firstActionId(secondLoad, "withdraw"));
  }

  @Test
  @DisplayName("Reassigned ids are saved and stay unchanged after a reload")
  void testReassignedIdsSurviveSaveAndReload() throws CommandSyntaxException {
    DialogDataSet dialogDataSet = loadDialogs();
    UniqueActionIdAssigner.assign(dialogDataSet, null, null);
    UUID reassignedId = firstActionId(dialogDataSet, "withdraw");

    DialogDataSet reloadedDialogDataSet = new DialogDataSet(dialogDataSet.createTag());
    UniqueActionIdAssigner.assign(reloadedDialogDataSet, null, null);

    assertEquals(reassignedId, firstActionId(reloadedDialogDataSet, "withdraw"));
  }

  @Test
  @DisplayName("Reassigning ids keeps the button order of the dialog")
  void testButtonOrderIsKept() throws CommandSyntaxException {
    DialogDataSet dialogDataSet = loadDialogs();
    dialogDataSet
        .getDialog("withdraw")
        .setDialogButton(new DialogButtonEntry("Last", "last", loadActions("say last")));
    dialogDataSet
        .getDialog("deposit")
        .setDialogButton(new DialogButtonEntry("Last", "last", loadActions("say last")));

    UniqueActionIdAssigner.assign(dialogDataSet, null, null);

    assertEquals(
        List.of("five", "last"),
        dialogDataSet.getDialog("withdraw").getDialogButtons().stream()
            .map(DialogButtonEntry::label)
            .toList());
  }

  @Test
  @DisplayName("Equal actions in dialogs, action events and trading offers get separate ids")
  void testEqualActionsAcrossContainersGetSeparateIds() throws CommandSyntaxException {
    DialogDataSet dialogDataSet = loadDialogs();
    ActionEventSet actionEventSet = new ActionEventSet();
    actionEventSet.setActionEvent(ActionEventType.ON_INTERACTION, loadActions("say bank"));
    actionEventSet.setActionEvent(ActionEventType.ON_DEATH, loadActions("say bank"));
    TradingDataSet tradingDataSet = new TradingDataSet();
    tradingDataSet.setOfferAction(0, loadActions("say bank"));

    UniqueActionIdAssigner.assign(dialogDataSet, actionEventSet, tradingDataSet);

    Set<UUID> actionIds = new HashSet<>();
    actionIds.add(firstActionId(dialogDataSet, "deposit"));
    actionIds.add(firstActionId(dialogDataSet, "withdraw"));
    actionIds.add(
        actionEventSet
            .getActionEvents(ActionEventType.ON_INTERACTION)
            .getOrderedEntries()
            .get(0)
            .id());
    actionIds.add(
        actionEventSet.getActionEvents(ActionEventType.ON_DEATH).getOrderedEntries().get(0).id());
    actionIds.add(tradingDataSet.getOfferAction(0).getOrderedEntries().get(0).id());
    assertEquals(5, actionIds.size());
  }

  @Test
  @DisplayName("Duplicate ids within one action list are made unique")
  void testDuplicateIdsWithinOneListAreMadeUnique() {
    ActionDataEntry actionDataEntry = new ActionDataEntry(ActionDataType.COMMAND, "say twice");
    ListTag actionDataList = new ListTag();
    actionDataList.add(actionDataEntry.createTag());
    actionDataList.add(actionDataEntry.withCommand("say again").createTag());
    ActionDataSet actionDataSet = new ActionDataSet().load(actionDataList);

    List<ActionDataEntry> uniqueEntries =
        actionDataSet.withUniqueIds(new HashSet<>(), "event:ON_INTERACTION").getOrderedEntries();

    assertEquals(actionDataEntry.id(), uniqueEntries.get(0).id());
    assertNotEquals(actionDataEntry.id(), uniqueEntries.get(1).id());
    assertEquals("say again", uniqueEntries.get(1).command());
  }

  @Test
  @DisplayName("An action list without duplicates is returned unchanged")
  void testUniqueActionListIsReturnedUnchanged() {
    ActionDataSet actionDataSet = loadActions("say one", "say two");

    assertSame(actionDataSet, actionDataSet.withUniqueIds(new HashSet<>(), "event:ON_DEATH"));
  }
}
