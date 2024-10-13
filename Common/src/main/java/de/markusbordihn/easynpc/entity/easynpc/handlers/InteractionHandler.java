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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationData;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.network.components.TextComponent;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
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
    if (!(player instanceof ServerPlayer serverPlayer) || hand != InteractionHand.MAIN_HAND) {
      return InteractionResult.PASS;
    }
    ConfigurationData<?> configurationData = easyNPC.getEasyNPCConfigurationData();
    OwnerData<?> ownerData = easyNPC.getEasyNPCOwnerData();
    boolean isOwnerOrCreative = serverPlayer.isCreative() || ownerData.isOwner(serverPlayer);

    // Item based actions.
    ItemStack handItemStack = player.getItemInHand(hand);
    if (!handItemStack.isEmpty()) {
      Item handItem = handItemStack.getItem();

      // Handle Easy NPC Wand
      Item easyNPCWand =
          BuiltInRegistries.ITEM
              .getOptional(ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "easy_npc_wand"))
              .orElse(null);
      if (handItem.equals(easyNPCWand)) {
        configurationData.openMainConfigurationMenu(serverPlayer);
        return InteractionResult.PASS;
      }

      // Handle Armourer's Workshop items like the NPC wand.
      if (Constants.MOD_ARMOURERS_WORKSHOP_ID.equals(
          BuiltInRegistries.ITEM.getKey(handItem).getNamespace())) {
        if (isOwnerOrCreative) {
          SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
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
    }

    // Open configuration menu for owner and creative mode if the player is crouching.
    if (player.isCreative() && player.isCrouching()) {
      configurationData.openMainConfigurationMenu(serverPlayer);
      return InteractionResult.PASS;
    }

    // Handle action event data.
    ActionEventData<?> actionEventData = easyNPC.getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.handleActionInteractionEvent(serverPlayer);
    }

    return InteractionResult.PASS;
  }
}
