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

package de.markusbordihn.easynpc.clienttest;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.By;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import java.io.IOException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class DialogScreenClientTest extends ClientTestBase {

  private static final String DIALOG_SCREEN_ID =
      "de.markusbordihn.easynpc.client.screen.dialog.dialogscreenwrapper";

  @Test
  @DisplayName("Dialog button resolves @npc and closes the dialog on click")
  void dialogButtonResolvesNpcNameAndCloses() throws IOException {
    command(
        "summon easy_npc:humanoid ~ ~ ~3 {Tags:[\""
            + TEST_TAG
            + "\"],CustomName:'\"Tester\"',DialogData:{Type:\"STANDARD\",DialogDataSet:[{"
            + "Label:\"default\",Texts:[{Text:\"Hi\"}],Buttons:[{Name:\"Talk to @npc\","
            + "Label:\"talk\",Actions:[{Type:\"CLOSE_DIALOG\"}]}]}]}}");
    command("easy_npc dialog open " + TEST_NPC_SELECTOR + " @s");

    client.await(Until.screen(DIALOG_SCREEN_ID), Until.widgetPresent(By.text("Talk to Tester")));
    captureScreen("dialog");
    client.click(By.text("Talk to Tester"), Until.NO_SCREEN_ID);
  }
}
