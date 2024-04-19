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

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionType;
import de.markusbordihn.easynpc.utils.TextFormattingCodes;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.UUID;
import net.minecraft.client.gui.Font;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;

public class DialogUtils {

  private static final String MACRO_NPC_STRING = "@npc";
  private static final String MACRO_INITIATOR_STRING = "@initiator";
  private static final int MAX_DIALOG_LINE_LENGTH = 178;
  private static final int MAX_SMALL_BUTTON_NAME_LENGTH = 20;

  protected DialogUtils() {}

  public static String parseDialogText(String text, LivingEntity entity, Player player) {

    // Handle dialog macros, if any.
    if (hasDialogMacros(text)) {
      // Replace entity macros.
      if (entity != null) {
        text = text.replace(MACRO_NPC_STRING, entity.getName().getString());
      }

      // Replace player macros.
      if (player != null) {
        text = text.replace(MACRO_INITIATOR_STRING, player.getName().getString());
      }
    }

    // Replace all line breaks macros.
    text = TextFormattingCodes.parseTextLineBreaks(text);

    // Replace color codes.
    text = TextFormattingCodes.parseTextFormattingCodes(text);

    return text;
  }

  public static boolean hasDialogMacros(String text) {
    return text != null
        && !text.isEmpty()
        && (text.contains(MACRO_NPC_STRING) || text.contains(MACRO_INITIATOR_STRING));
  }

  public static String generateButtonLabel(String name) {
    return generateLabel(name, "button", DialogButtonData.MAX_BUTTON_LABEL_LENGTH);
  }

  public static String generateDialogLabel(String name) {
    return generateLabel(name, "dialog", DialogDataEntry.MAX_DIALOG_LABEL_LENGTH);
  }

  private static String generateLabel(String name, String type, int maxLength) {
    if (name == null || name.isEmpty()) {
      // Generate random label name
      return type
          + "_"
          + UUID.randomUUID().toString().substring(0, 8).replace("-", "").toLowerCase();
    }
    String label = name.trim().toLowerCase();
    label = label.replace(" ", "_");
    label = label.replaceAll("[^a-z0-9_]", "");
    return label.length() > maxLength ? label.substring(0, maxLength) : label;
  }

  public static int getNumbersOfDialogLines(String text, Font font) {
    return getNumbersOfDialogLines(text, MAX_DIALOG_LINE_LENGTH, font);
  }

  public static int getNumbersOfDialogLines(String text, int maxLineLength, Font font) {
    if (text == null || text.isEmpty()) {
      return 0;
    }
    Component textComponent = Component.literal(text);
    return font.split(textComponent, maxLineLength).size();
  }

  public static DialogDataSet getBasicDialog(String dialog) {
    DialogDataSet dialogDataSet = new DialogDataSet(DialogType.BASIC);
    DialogDataEntry dialogData = new DialogDataEntry("Basic Dialog", dialog, false);
    dialogDataSet.addDialog(dialogData);
    return dialogDataSet;
  }

  public static DialogDataSet getYesNoDialog(
      String dialogText,
      String yesButtonText,
      String noButtonText,
      String yesDialogText,
      String noDialogText) {

    // Define yes and no actions.
    HashSet<ActionDataEntry> yesActionDatumEntries = new HashSet<>();
    yesActionDatumEntries.add(new ActionDataEntry(ActionType.OPEN_NAMED_DIALOG, "yes_answer"));
    HashSet<ActionDataEntry> noActionDatumEntries = new HashSet<>();
    noActionDatumEntries.add(new ActionDataEntry(ActionType.OPEN_NAMED_DIALOG, "no_answer"));

    // Define yes and no buttons.
    DialogButtonData yesButtonData =
        new DialogButtonData(yesButtonText, "yes_button", yesActionDatumEntries);
    DialogButtonData noButtonData =
        new DialogButtonData(noButtonText, "no_button", noActionDatumEntries);

    // Define list of buttons for the dialog.
    Set<DialogButtonData> buttons = new LinkedHashSet<>();
    buttons.add(yesButtonData);
    buttons.add(noButtonData);

    // Build dialog data set.
    DialogDataSet dialogDataSet = new DialogDataSet(DialogType.YES_NO);
    dialogDataSet.addDefaultDialog(
        new DialogDataEntry("question", "Question Dialog", dialogText, false, buttons));
    dialogDataSet.addDialog(new DialogDataEntry("yes_answer", "Yes Dialog", yesDialogText, false));
    dialogDataSet.addDialog(new DialogDataEntry("no_answer", "No Dialog", noDialogText, false));
    return dialogDataSet;
  }

  public static DialogScreenLayout getDialogScreenLayout(DialogDataEntry dialogData, Font font) {
    if (dialogData == null) {
      return DialogScreenLayout.UNKNOWN;
    }
    boolean hasText = !dialogData.getText().isBlank();
    int numberOfButtons = dialogData.getNumberOfButtons();

    if (!hasText) {
      return DialogScreenLayout.UNKNOWN;
    }

    // Check if we could use a compact layout or if we need to use a full layout.
    String dialogText = dialogData.getDialogText();
    boolean hasDialogMacros = hasDialogMacros(dialogText);

    // Check if we need to parse line breaks.
    if (TextFormattingCodes.hasTextLinebreakCodes(dialogText)) {
      dialogText = TextFormattingCodes.parseTextLineBreaks(dialogText);
    } else if (hasDialogMacros) {
      dialogText = dialogText + "PLACEHOLDER_FOR_POSSIBLE_MACROS";
    }

    // Calculate the number of lines.
    int numberOfLines = getNumbersOfDialogLines(dialogText, font);

    // Get the max length of the button names to check if we could use a compact layout.
    int maxButtonNameLength = 0;
    if (numberOfButtons > 0) {
      for (DialogButtonData buttonData : dialogData.getButtons()) {
        int buttonNameLength = buttonData.getName().length();
        if (buttonNameLength > maxButtonNameLength) {
          maxButtonNameLength = buttonNameLength;
        }
      }
    }
    boolean hasLargeButtonName = maxButtonNameLength > MAX_SMALL_BUTTON_NAME_LENGTH;

    // Everything with 6 or fewer lines could be displayed in a compact layout.
    if (numberOfLines <= 6) {
      if (numberOfButtons == 0) {
        return DialogScreenLayout.COMPACT_TEXT_ONLY;
      } else if (numberOfButtons == 1) {
        return DialogScreenLayout.COMPACT_TEXT_WITH_ONE_BUTTON;
      } else if (numberOfButtons == 2) {
        return hasLargeButtonName
            ? DialogScreenLayout.COMPACT_TEXT_WITH_TWO_LARGE_BUTTONS
            : DialogScreenLayout.COMPACT_TEXT_WITH_TWO_BUTTONS;
      } else if (numberOfButtons == 3) {
        return DialogScreenLayout.COMPACT_TEXT_WITH_THREE_BUTTONS;
      } else if (numberOfButtons == 4) {
        return DialogScreenLayout.COMPACT_TEXT_WITH_FOUR_BUTTONS;
      } else if (numberOfButtons == 5) {
        return DialogScreenLayout.COMPACT_TEXT_WITH_FIVE_BUTTONS;
      } else if (numberOfButtons == 6) {
        return DialogScreenLayout.COMPACT_TEXT_WITH_SIX_BUTTONS;
      }
    }

    // Everything else will be displayed in a full layout.
    if (numberOfButtons == 0) {
      return DialogScreenLayout.TEXT_ONLY;
    } else if (numberOfButtons == 1) {
      return DialogScreenLayout.TEXT_WITH_ONE_BUTTON;
    } else if (numberOfButtons == 2) {
      return DialogScreenLayout.TEXT_WITH_TWO_BUTTONS;
    } else if (numberOfButtons == 3) {
      return DialogScreenLayout.TEXT_WITH_THREE_BUTTONS;
    } else if (numberOfButtons == 4) {
      return DialogScreenLayout.TEXT_WITH_FOUR_BUTTONS;
    } else if (numberOfButtons == 5) {
      return DialogScreenLayout.TEXT_WITH_FIVE_BUTTONS;
    } else if (numberOfButtons == 6) {
      return DialogScreenLayout.TEXT_WITH_SIX_BUTTONS;
    }

    // Fallback to unknown layout.
    return DialogScreenLayout.UNKNOWN;
  }
}
