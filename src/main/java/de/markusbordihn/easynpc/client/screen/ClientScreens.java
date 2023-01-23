/**
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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.client.gui.screens.MenuScreens;

import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.configuration.BasicDialogConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.DialogConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.MainConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.YesNoDialogConfigurationScreen;
import de.markusbordihn.easynpc.menu.ModMenuTypes;

public class ClientScreens {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected ClientScreens() {}

  public static void registerScreens(final FMLClientSetupEvent event) {
    log.info("{} Client Screens ...", Constants.LOG_REGISTER_PREFIX);

    event.enqueueWork(() -> {
      MenuScreens.register(ModMenuTypes.CONFIGURATION_MENU.get(), ConfigurationScreen::new);
      MenuScreens.register(ModMenuTypes.DIALOG_MENU.get(), DialogScreen::new);

      // Configuration Screens
      MenuScreens.register(ModMenuTypes.MAIN_CONFIGURATION_MENU.get(),
          MainConfigurationScreen::new);

      MenuScreens.register(ModMenuTypes.DIALOG_CONFIGURATION_MENU.get(),
          DialogConfigurationScreen::new);
      MenuScreens.register(ModMenuTypes.BASIC_DIALOG_CONFIGURATION_MENU.get(),
          BasicDialogConfigurationScreen::new);
      MenuScreens.register(ModMenuTypes.YES_NO_DIALOG_CONFIGURATION_MENU.get(),
          YesNoDialogConfigurationScreen::new);


    });
  }
}
