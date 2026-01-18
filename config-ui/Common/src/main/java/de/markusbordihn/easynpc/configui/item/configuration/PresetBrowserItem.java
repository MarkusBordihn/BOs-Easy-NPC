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

package de.markusbordihn.easynpc.configui.item.configuration;

import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.data.custom.CustomMenuType;
import de.markusbordihn.easynpc.configui.menu.MenuManager;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.function.Consumer;
import net.minecraft.core.registries.Registries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.component.TooltipDisplay;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PresetBrowserItem extends Item {

  public static final String ID = "preset_browser";
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public PresetBrowserItem(Properties properties) {
    super(
        properties.setId(
            ResourceKey.create(
                Registries.ITEM, Identifier.fromNamespaceAndPath(Constants.MOD_ID, ID))));
  }

  @Override
  public InteractionResult use(Level level, Player player, InteractionHand hand) {
    if (!level.isClientSide() && player instanceof ServerPlayer serverPlayer) {
      MenuManager.getMenuHandler().openCustomMenu(CustomMenuType.PRESET_BROWSER, serverPlayer);
      return InteractionResult.SUCCESS;
    }
    return InteractionResult.PASS;
  }

  @Override
  public void appendHoverText(
      ItemStack itemStack,
      TooltipContext tooltipContext,
      TooltipDisplay tooltipDisplay,
      Consumer<Component> consumer,
      TooltipFlag flag) {
    consumer.accept(TextComponent.getTranslatedConfigText(ID + ".tooltip"));
  }
}
