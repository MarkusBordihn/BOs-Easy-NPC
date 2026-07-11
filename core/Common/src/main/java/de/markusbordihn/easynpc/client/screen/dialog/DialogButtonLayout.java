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

package de.markusbordihn.easynpc.client.screen.dialog;

import de.markusbordihn.easynpc.data.dialog.DialogScreenLayout;
import java.util.List;
import net.minecraft.client.gui.components.Button;

public final class DialogButtonLayout {

  private static final int BUTTON_WIDTH = 134;
  private static final int MIDDLE_BUTTON_WIDTH = 208;
  private static final int LARGE_BUTTON_WIDTH = 262;

  private DialogButtonLayout() {}

  public static boolean apply(
      DialogScreenLayout dialogScreenLayout,
      List<Button> visibleDialogButtons,
      int leftPos,
      int topPos) {
    switch (dialogScreenLayout) {
      case UNKNOWN, COMPACT_TEXT_ONLY, TEXT_ONLY:
        return true;
      case COMPACT_TEXT_WITH_ONE_BUTTON:
        position(visibleDialogButtons.get(0), LARGE_BUTTON_WIDTH, leftPos + 18, topPos + 140);
        return true;
      case COMPACT_TEXT_WITH_TWO_BUTTONS:
        Button firstCompactDialogButton =
            position(visibleDialogButtons.get(0), BUTTON_WIDTH, leftPos + 10, topPos + 140);
        position(
            visibleDialogButtons.get(1),
            BUTTON_WIDTH,
            firstCompactDialogButton.getX() + firstCompactDialogButton.getWidth() + 9,
            firstCompactDialogButton.getY());
        return true;
      case COMPACT_TEXT_WITH_TWO_LARGE_BUTTONS:
        Button firstCompactLargeDialogButton =
            position(visibleDialogButtons.get(0), MIDDLE_BUTTON_WIDTH, leftPos + 75, topPos + 115);
        position(
            visibleDialogButtons.get(1),
            MIDDLE_BUTTON_WIDTH,
            firstCompactLargeDialogButton.getX(),
            firstCompactLargeDialogButton.getY() + firstCompactLargeDialogButton.getHeight() + 9);
        return true;
      case TEXT_WITH_ONE_BUTTON:
        position(visibleDialogButtons.get(0), LARGE_BUTTON_WIDTH, leftPos + 18, topPos + 170);
        return true;
      case TEXT_WITH_TWO_BUTTONS:
        Button firstTwoDialogButton =
            position(visibleDialogButtons.get(0), LARGE_BUTTON_WIDTH, leftPos + 18, topPos + 159);
        position(
            visibleDialogButtons.get(1),
            LARGE_BUTTON_WIDTH,
            firstTwoDialogButton.getX(),
            firstTwoDialogButton.getY() + firstTwoDialogButton.getHeight() + 9);
        return true;
      case COMPACT_TEXT_WITH_THREE_BUTTONS, TEXT_WITH_THREE_BUTTONS:
        Button firstThreeDialogButton =
            position(visibleDialogButtons.get(0), LARGE_BUTTON_WIDTH, leftPos + 18, topPos + 154);
        Button secondThreeDialogButton =
            position(
                visibleDialogButtons.get(1),
                LARGE_BUTTON_WIDTH,
                firstThreeDialogButton.getX(),
                firstThreeDialogButton.getY() + firstThreeDialogButton.getHeight() + 4);
        position(
            visibleDialogButtons.get(2),
            LARGE_BUTTON_WIDTH,
            secondThreeDialogButton.getX(),
            secondThreeDialogButton.getY() + secondThreeDialogButton.getHeight() + 4);
        return true;
      case COMPACT_TEXT_WITH_FOUR_BUTTONS, TEXT_WITH_FOUR_BUTTONS:
        Button firstFourDialogButton =
            position(visibleDialogButtons.get(0), BUTTON_WIDTH, leftPos + 10, topPos + 164);
        Button secondFourDialogButton =
            position(
                visibleDialogButtons.get(1),
                BUTTON_WIDTH,
                firstFourDialogButton.getX() + firstFourDialogButton.getWidth() + 9,
                firstFourDialogButton.getY());
        Button thirdFourDialogButton =
            position(
                visibleDialogButtons.get(2),
                BUTTON_WIDTH,
                firstFourDialogButton.getX(),
                firstFourDialogButton.getY() + firstFourDialogButton.getHeight() + 9);
        position(
            visibleDialogButtons.get(3),
            BUTTON_WIDTH,
            secondFourDialogButton.getX(),
            thirdFourDialogButton.getY());
        return true;
      case COMPACT_TEXT_WITH_FIVE_BUTTONS, TEXT_WITH_FIVE_BUTTONS:
        Button firstFiveDialogButton =
            position(visibleDialogButtons.get(0), BUTTON_WIDTH, leftPos + 10, topPos + 154);
        Button secondFiveDialogButton =
            position(
                visibleDialogButtons.get(1),
                BUTTON_WIDTH,
                firstFiveDialogButton.getX() + firstFiveDialogButton.getWidth() + 9,
                firstFiveDialogButton.getY());
        Button thirdFiveDialogButton =
            position(
                visibleDialogButtons.get(2),
                BUTTON_WIDTH,
                firstFiveDialogButton.getX(),
                firstFiveDialogButton.getY() + firstFiveDialogButton.getHeight() + 4);
        position(
            visibleDialogButtons.get(3),
            BUTTON_WIDTH,
            secondFiveDialogButton.getX(),
            thirdFiveDialogButton.getY());
        position(
            visibleDialogButtons.get(4),
            BUTTON_WIDTH,
            firstFiveDialogButton.getX(),
            thirdFiveDialogButton.getY() + thirdFiveDialogButton.getHeight() + 4);
        return true;
      case COMPACT_TEXT_WITH_SIX_BUTTONS, TEXT_WITH_SIX_BUTTONS:
        Button firstSixDialogButton =
            position(visibleDialogButtons.get(0), BUTTON_WIDTH, leftPos + 10, topPos + 154);
        Button secondSixDialogButton =
            position(
                visibleDialogButtons.get(1),
                BUTTON_WIDTH,
                firstSixDialogButton.getX() + firstSixDialogButton.getWidth() + 9,
                firstSixDialogButton.getY());
        Button thirdSixDialogButton =
            position(
                visibleDialogButtons.get(2),
                BUTTON_WIDTH,
                firstSixDialogButton.getX(),
                firstSixDialogButton.getY() + firstSixDialogButton.getHeight() + 4);
        position(
            visibleDialogButtons.get(3),
            BUTTON_WIDTH,
            secondSixDialogButton.getX(),
            thirdSixDialogButton.getY());
        Button fifthSixDialogButton =
            position(
                visibleDialogButtons.get(4),
                BUTTON_WIDTH,
                firstSixDialogButton.getX(),
                thirdSixDialogButton.getY() + thirdSixDialogButton.getHeight() + 4);
        position(
            visibleDialogButtons.get(5),
            BUTTON_WIDTH,
            secondSixDialogButton.getX(),
            fifthSixDialogButton.getY());
        return true;
      default:
        return false;
    }
  }

  private static Button position(Button dialogButton, int width, int left, int top) {
    dialogButton.setWidth(width);
    dialogButton.setX(left);
    dialogButton.setY(top);
    return dialogButton;
  }
}
