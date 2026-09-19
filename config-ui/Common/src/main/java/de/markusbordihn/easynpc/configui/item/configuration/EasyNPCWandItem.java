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

import de.markusbordihn.easynpc.config.SecurityConfig;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.menu.MenuManager;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPCBase;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.network.message.client.HighlightEasyNPCMessage;
import de.markusbordihn.easynpc.security.CommandSecurity;
import de.markusbordihn.easynpc.security.FeatureSecurity;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.Registries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.util.Mth;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.component.TooltipDisplay;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.AABB;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCWandItem extends Item {

  public static final String ID = "easy_npc_wand";
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final int HIGHLIGHT_DURATION_TICKS = 4 * 20;
  private static final int HIGHLIGHT_INTERVAL_TICKS = 30;
  private static final double HIGHLIGHT_RADIUS = 32.0d;
  private static final double HIGHLIGHT_RECEIVER_RADIUS = 64.0d;

  public EasyNPCWandItem(Properties properties) {
    super(
        properties.setId(
            ResourceKey.create(
                Registries.ITEM, Identifier.fromNamespaceAndPath(Constants.MOD_ID, ID))));
  }

  private static void sendHighlight(ServerPlayer serverPlayer, List<Mob> easyNPCs) {
    if (easyNPCs.isEmpty()) {
      return;
    }

    NetworkHandlerManager.sendMessageToPlayer(
        new HighlightEasyNPCMessage(
            easyNPCs.stream().map(Entity::getUUID).toList(), HIGHLIGHT_DURATION_TICKS),
        serverPlayer);
  }

  private static List<Mob> getOwnedEasyNPCs(List<Mob> easyNPCs, ServerPlayer serverPlayer) {
    List<Mob> ownedEasyNPCs = new ArrayList<>();
    for (Mob mob : easyNPCs) {
      OwnerDataCapable<?> ownerData = ((EasyNPCBase<?>) mob).getEasyNPCOwnerData();
      if (ownerData != null && serverPlayer.getUUID().equals(ownerData.getOwnerUUID())) {
        ownedEasyNPCs.add(mob);
      }
    }

    return ownedEasyNPCs;
  }

  @Override
  public void inventoryTick(
      ItemStack itemStack, ServerLevel serverLevel, Entity entity, EquipmentSlot equipmentSlot) {
    if (!itemStack.is(this)
        || !(entity instanceof ServerPlayer serverPlayer)
        || serverPlayer.containerMenu instanceof ConfigurationMenu
        || (equipmentSlot != EquipmentSlot.MAINHAND && equipmentSlot != EquipmentSlot.OFFHAND)
        || serverLevel.getGameTime() % HIGHLIGHT_INTERVAL_TICKS != 0) {
      return;
    }

    List<Mob> easyNPCs =
        serverLevel.getEntitiesOfClass(
            Mob.class,
            serverPlayer.getBoundingBox().inflate(HIGHLIGHT_RADIUS),
            mob -> mob.isAlive() && mob instanceof EasyNPCBase<?>);
    if (easyNPCs.isEmpty()) {
      return;
    }

    sendHighlight(serverPlayer, easyNPCs);
    for (ServerPlayer nearbyPlayer :
        serverLevel.getEntitiesOfClass(
            ServerPlayer.class,
            serverPlayer.getBoundingBox().inflate(HIGHLIGHT_RECEIVER_RADIUS),
            otherPlayer -> otherPlayer != serverPlayer)) {
      if (FeatureSecurity.getRole(CommandSecurity.getActorContext(nearbyPlayer))
          .allows(SecurityConfig.NPC_HIGHLIGHT_MINIMUM_ROLE)) {
        sendHighlight(nearbyPlayer, easyNPCs);
      } else if (SecurityConfig.NPC_HIGHLIGHT_FOR_OWNER) {
        sendHighlight(nearbyPlayer, getOwnedEasyNPCs(easyNPCs, nearbyPlayer));
      }
    }
  }

  @Override
  public InteractionResult interactLivingEntity(
      ItemStack itemStack,
      Player player,
      LivingEntity livingEntity,
      InteractionHand interactionHand) {
    if (player instanceof ServerPlayer serverPlayer
        && livingEntity instanceof EasyNPCBase<?> easyNPCEntity) {

      // Shift+right-click: Quick rotate NPC to face the player.
      if (player.isShiftKeyDown()) {
        ModelDataCapable<?> modelData = easyNPCEntity.getEasyNPCModelData();
        if (modelData != null) {
          double dx = player.getX() - livingEntity.getX();
          double dz = player.getZ() - livingEntity.getZ();
          float yaw = (float) Math.toDegrees(Mth.atan2(dz, dx)) - 90.0f;
          yaw = Mth.wrapDegrees(yaw);
          modelData.setModelRotation(yaw);
          serverPlayer.sendOverlayMessage(
              Component.literal("Rotation: " + String.format("%.1f", yaw) + "°"));
        }
        return InteractionResult.SUCCESS;
      }

      MenuManager.getMenuHandler()
          .openConfigurationMenu(ConfigurationType.MAIN, serverPlayer, easyNPCEntity, 0);
      return InteractionResult.SUCCESS;
    }
    return InteractionResult.PASS;
  }

  @Override
  public InteractionResult useOn(UseOnContext userContext) {
    Level level = userContext.getLevel();
    Player player = userContext.getPlayer();
    if (player instanceof ServerPlayer serverPlayer) {
      BlockPos blockPos = userContext.getClickedPos();

      // Search for nearby EasyNPC entities above and below the block position
      AABB aabbAbove =
          new AABB(
              blockPos.getX() - 0.25d,
              blockPos.getY() - 2d,
              blockPos.getZ() - 0.25d,
              blockPos.getX() + 0.25d,
              blockPos.getY() + 2d,
              blockPos.getZ() + 0.25d);
      for (Mob mob : level.getEntitiesOfClass(Mob.class, aabbAbove.inflate(0.5), Entity::isAlive)) {
        if (mob instanceof EasyNPCBase<?> easyNPC) {
          MenuManager.getMenuHandler()
              .openConfigurationMenu(ConfigurationType.MAIN, serverPlayer, easyNPC, 0);
          return InteractionResult.SUCCESS;
        }
      }

      // Search for nearby EasyNPC entities around the block position
      AABB aabbAround =
          new AABB(
              blockPos.getX() - 0.5d,
              blockPos.getY() - 0.5d,
              blockPos.getZ() - 0.5d,
              blockPos.getX() + 1d,
              blockPos.getY() + 1d,
              blockPos.getZ() + 1d);
      for (Mob mob :
          level.getEntitiesOfClass(Mob.class, aabbAround.inflate(0.5), Entity::isAlive)) {
        if (mob instanceof EasyNPCBase<?> easyNPC) {
          MenuManager.getMenuHandler()
              .openConfigurationMenu(ConfigurationType.MAIN, serverPlayer, easyNPC, 0);
          return InteractionResult.SUCCESS;
        }
      }

      // Expand the search area to find all nearby EasyNPC entities
      for (Mob mob :
          level.getEntitiesOfClass(Mob.class, aabbAround.inflate(2.5), Entity::isAlive)) {
        if (mob instanceof EasyNPCBase<?> easyNPC) {
          MenuManager.getMenuHandler()
              .openConfigurationMenu(ConfigurationType.MAIN, serverPlayer, easyNPC, 0);
          return InteractionResult.SUCCESS;
        }
      }
    }
    return InteractionResult.PASS;
  }

  @Override
  public boolean isFoil(ItemStack itemStack) {
    return true;
  }

  @Override
  public void appendHoverText(
      ItemStack itemStack,
      TooltipContext tooltipContext,
      TooltipDisplay tooltipDisplay,
      Consumer<Component> consumer,
      TooltipFlag tooltipFlag) {
    consumer.accept(TextComponent.getTranslatedTextRaw(Constants.TEXT_ITEM_PREFIX + ID));
    consumer.accept(
        TextComponent.getTranslatedTextRaw(Constants.TEXT_ITEM_PREFIX + ID + ".rotation_hint"));
  }
}
