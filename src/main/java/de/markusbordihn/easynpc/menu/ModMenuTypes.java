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

package de.markusbordihn.easynpc.menu;

import net.minecraft.world.inventory.MenuType;

import net.minecraftforge.common.extensions.IForgeMenuType;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.RegistryObject;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.menu.configuration.BasicDialogConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.DialogConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.MainConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.YesNoDialogConfigurationMenu;

public class ModMenuTypes {

  protected ModMenuTypes() {

  }

  public static final DeferredRegister<MenuType<?>> MENU_TYPES =
      DeferredRegister.create(ForgeRegistries.MENU_TYPES, Constants.MOD_ID);

  // Dialog
  public static final RegistryObject<MenuType<DialogMenu>> DIALOG_MENU =
      MENU_TYPES.register("dialog_menu", () -> IForgeMenuType.create(DialogMenu::new));

  // Configuration
  public static final RegistryObject<MenuType<DialogConfigurationMenu>> DIALOG_CONFIGURATION_MENU =
      MENU_TYPES.register("dialog_configuration_menu",
          () -> IForgeMenuType.create(DialogConfigurationMenu::new));
  public static final RegistryObject<MenuType<BasicDialogConfigurationMenu>> BASIC_DIALOG_CONFIGURATION_MENU =
      MENU_TYPES.register("basic_dialog_configuration_menu",
          () -> IForgeMenuType.create(BasicDialogConfigurationMenu::new));
  public static final RegistryObject<MenuType<YesNoDialogConfigurationMenu>> YES_NO_DIALOG_CONFIGURATION_MENU =
      MENU_TYPES.register("yes_no_dialog_configuration_menu",
          () -> IForgeMenuType.create(YesNoDialogConfigurationMenu::new));
  public static final RegistryObject<MenuType<MainConfigurationMenu>> MAIN_CONFIGURATION_MENU =
      MENU_TYPES.register("main_configuration_menu",
          () -> IForgeMenuType.create(MainConfigurationMenu::new));

  public static final RegistryObject<MenuType<ConfigurationMenu>> CONFIGURATION_MENU = MENU_TYPES
      .register("configuration_menu", () -> IForgeMenuType.create(ConfigurationMenu::new));
}
