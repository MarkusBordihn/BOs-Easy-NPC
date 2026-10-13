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

package de.markusbordihn.easynpc.entity.easynpc.handlers;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.ItemUtils;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class InteractionHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private InteractionHandler() {}

  public static InteractionResult handleMobInteraction(
      EasyNPC<?> easyNPC, Player player, InteractionHand hand) {
    if (hand != InteractionHand.MAIN_HAND) {
      return InteractionResult.PASS;
    }

    ItemStack handItemStack = player.getItemInHand(hand);
    Item handItem = handItemStack.getItem();
    if (isEasyNPCToolItem(handItem)) {
      return InteractionResult.PASS;
    }

    boolean isArmourersWorkshopItem =
        Constants.MOD_ARMOURERS_WORKSHOP_ID.equals(
            BuiltInRegistries.ITEM.getKey(handItem).getNamespace());
    if (!(player instanceof ServerPlayer serverPlayer)) {
      return isArmourersWorkshopItem ? InteractionResult.PASS : InteractionResult.SUCCESS;
    }

    if (isArmourersWorkshopItem) {
      OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
      if (serverPlayer.isCreative() || ownerData.isNPCOwner(serverPlayer)) {
        SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
        if (skinData.getSkinModel().hasArmourersWorkshopSupport()) {
          log.debug("Ignore event for Armourer's Workshop Item for {} ...", easyNPC);
          return InteractionResult.PASS;
        } else {
          serverPlayer.sendSystemMessage(
              TextComponent.getTranslatedText(
                  "armourers_workshop.no_support",
                  skinData.getSkinModel().name(),
                  easyNPC.toString()));
        }
      } else {
        log.debug(
            "{} has no permissions to use Armourer's Workshop Item for {} ...",
            serverPlayer,
            easyNPC);
        return InteractionResult.CONSUME;
      }
    }

    DisplayAttributeDataCapable<?> displayAttributeData = easyNPC.getEasyNPCDisplayAttributeData();
    boolean blockWhenInvisible =
        displayAttributeData != null
            && displayAttributeData.hasDisplayAttribute(
                DisplayAttributeType.INTERACTION_WHEN_INVISIBLE)
            && !displayAttributeData.getDisplayBooleanAttribute(
                DisplayAttributeType.INTERACTION_WHEN_INVISIBLE);
    if (blockWhenInvisible
        && VisibilityHandler.handleIsInvisibleToPlayer(easyNPC, serverPlayer, false)) {
      return InteractionResult.PASS;
    }

    ActionEventDataCapable<?> actionEventData = easyNPC.getEasyNPCActionEventData();
    if (actionEventData == null
        || !actionEventData.hasActionEvent(ActionEventType.ON_INTERACTION)) {
      return InteractionResult.PASS;
    }

    actionEventData.handleActionEvent(ActionEventType.ON_INTERACTION, serverPlayer);
    return InteractionResult.CONSUME;
  }

  private static boolean isEasyNPCToolItem(Item item) {
    return item.equals(ItemUtils.getEasyNPCWandItem())
        || item.equals(ItemUtils.getMoveEasyNPCItem())
        || item.equals(ItemUtils.getEasyNPCPresetEmptyItem())
        || item.equals(ItemUtils.getEasyNPCPresetItem());
  }
}
