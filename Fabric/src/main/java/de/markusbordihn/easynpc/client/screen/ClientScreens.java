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

package de.markusbordihn.easynpc.client.screen;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.dialog.DialogScreenWrapper;
import de.markusbordihn.easynpc.client.screen.spawner.SpawnerScreenWrapper;
import de.markusbordihn.easynpc.client.screen.testing.TestScreenWrapper;
import de.markusbordihn.easynpc.menu.ModMenuTypes;
import net.minecraft.client.gui.screens.MenuScreens;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ClientScreens {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ClientScreens() {
  }

  public static void registerScreens() {
    log.info("{} Client Screens ...", Constants.LOG_REGISTER_PREFIX);

    MenuScreens.register(ModMenuTypes.DIALOG_MENU, DialogScreenWrapper::new);
    MenuScreens.register(ModMenuTypes.SPAWNER_MENU, SpawnerScreenWrapper::new);

    MenuScreens.register(ModMenuTypes.TEST_MENU, TestScreenWrapper::new);
  }
}
