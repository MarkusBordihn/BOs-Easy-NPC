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

package de.markusbordihn.easynpc.menu;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.menu.dialog.DialogMenuWrapper;
import de.markusbordihn.easynpc.menu.spawner.SpawnerMenuWrapper;
import de.markusbordihn.easynpc.menu.testing.TestMenuWrapper;
import net.fabricmc.fabric.api.screenhandler.v1.ScreenHandlerRegistry;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.inventory.MenuType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ModMenuTypes {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ModMenuTypes() {
  }

  public static void register() {
    log.info("{} Menu Types ...", Constants.LOG_REGISTER_PREFIX);
  }

  public static final MenuType<DialogMenuWrapper> DIALOG_MENU =
      ScreenHandlerRegistry.registerSimple(
          new ResourceLocation(Constants.MOD_ID, "dialog_menu"), DialogMenuWrapper::new);

  public static final MenuType<SpawnerMenuWrapper> SPAWNER_MENU =
      ScreenHandlerRegistry.registerSimple(
          new ResourceLocation(Constants.MOD_ID, "spawner_menu"), SpawnerMenuWrapper::new);

  public static final MenuType<TestMenuWrapper> TEST_MENU =
      ScreenHandlerRegistry.registerSimple(
          new ResourceLocation(Constants.MOD_ID, "test_menu"), TestMenuWrapper::new);
}
