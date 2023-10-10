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

package de.markusbordihn.easynpc.entity;

import java.util.List;
import javax.annotation.Nullable;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.DifficultyInstance;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.entity.SpawnGroupData;
import net.minecraft.world.entity.animal.FlyingAnimal;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.ServerLevelAccessor;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.Vec3;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.commands.CommandManager;
import de.markusbordihn.easynpc.data.action.ActionData;
import de.markusbordihn.easynpc.data.action.ActionGroup;
import de.markusbordihn.easynpc.data.action.ActionManager;
import de.markusbordihn.easynpc.data.action.ActionType;
import de.markusbordihn.easynpc.entity.ai.goal.CustomLookAtPlayerGoal;
import de.markusbordihn.easynpc.entity.ai.goal.ResetLookAtPlayerGoal;
import de.markusbordihn.easynpc.item.ModItems;

public class EasyNPCEntity extends EasyNPCEntityData {

  // Additional ticker
  private static final int BASE_TICK = 16;
  private static final int TRAVEL_TICK = 20;
  private int baseTicker = random.nextInt(BASE_TICK / 2);
  private int travelTicker = random.nextInt(TRAVEL_TICK / 2);

  // Shared constants
  public static final MobCategory CATEGORY = MobCategory.MISC;

  public EasyNPCEntity(EntityType<? extends EasyNPCEntity> entityType, Level level,
      Enum<?> variant) {
    this(entityType, level);
    this.setVariant(variant);

    // Distribute Ticks along several entities.
    this.baseTicker = this.random.nextInt(0, BASE_TICK / 2);
  }

  public EasyNPCEntity(EntityType<? extends EasyNPCEntity> entityType, Level level) {
    super(entityType, level);
    this.setInvulnerable(true);
  }

  public void finalizeSpawn() {
    // Do stuff like default names.
  }

  public void executeAction(ActionData actionData, ServerPlayer serverPlayer) {
    if (actionData == null || !actionData.isValidAndNotEmpty()) {
      return;
    }
    int permissionLevel = this.getActionPermissionLevel();
    log.debug("Execute action {} for {} with permission level {} ...", actionData, this,
        permissionLevel);
    if (actionData.shouldExecuteAsUser()) {
      // Execute action as user with define permission level (default 0).
      CommandManager.executePlayerCommand(actionData.getAction(this, serverPlayer), serverPlayer,
          actionData.getPermissionLevel(), actionData.isDebugEnabled());
    } else {
      // Execute action as NPC entity with owner permission level.
      CommandManager.executeEntityCommand(actionData.getAction(this, serverPlayer), this,
          permissionLevel, actionData.isDebugEnabled());
    }
  }

  public void openMainConfigurationMenu(ServerPlayer serverPlayer) {
    EasyNPCEntityMenu.openMainConfigurationMenu(serverPlayer, this);
  }

  public void npcBaseTick() {
    this.level.getProfiler().push("npcBaseTick");

    // Check distance for additional actions.
    checkDistanceActions();

    this.level.getProfiler().pop();
  }

  public void checkDistanceActions() {
    this.level.getProfiler().push("npcCheckDistanceActions");
    // Check to avoid additional checks, when no player is in range.
    boolean skipPlayerDistanceCheck = false;

    // Near distance action, if set.
    if (this.hasAction(ActionType.ON_DISTANCE_NEAR)) {
      List<Player> listOfPlayers = this.getPlayersInRange(16.0D);
      if (listOfPlayers == null || listOfPlayers.isEmpty()) {
        ActionManager.removeActionGroup(this, ActionGroup.DISTANCE_NEAR);
        skipPlayerDistanceCheck = true;
      } else {
        ActionData actionData = this.getActionData(ActionType.ON_DISTANCE_NEAR);
        for (Player player : listOfPlayers) {
          if (player instanceof ServerPlayer serverPlayer
              && !ActionManager.containsPlayer(this, ActionGroup.DISTANCE_NEAR, serverPlayer)) {
            this.executeAction(actionData, serverPlayer);
            ActionManager.addPlayer(this, ActionGroup.DISTANCE_NEAR, serverPlayer);
          }
        }
      }
    }

    // Close distance action, if set.
    if (this.hasAction(ActionType.ON_DISTANCE_CLOSE)) {
      List<Player> listOfPlayers = skipPlayerDistanceCheck ? null : this.getPlayersInRange(8.0D);
      if (listOfPlayers == null || listOfPlayers.isEmpty()) {
        ActionManager.removeActionGroup(this, ActionGroup.DISTANCE_CLOSE);
        skipPlayerDistanceCheck = true;
      } else {
        ActionData actionData = this.getActionData(ActionType.ON_DISTANCE_CLOSE);
        for (Player player : listOfPlayers) {
          if (player instanceof ServerPlayer serverPlayer
              && !ActionManager.containsPlayer(this, ActionGroup.DISTANCE_CLOSE, serverPlayer)) {
            this.executeAction(actionData, serverPlayer);
            ActionManager.addPlayer(this, ActionGroup.DISTANCE_CLOSE, serverPlayer);
          }
        }
      }
    }

    // Very close distance action, if set.
    if (this.hasAction(ActionType.ON_DISTANCE_VERY_CLOSE)) {
      List<Player> listOfPlayers = skipPlayerDistanceCheck ? null : this.getPlayersInRange(4.0D);
      if (listOfPlayers == null || listOfPlayers.isEmpty()) {
        ActionManager.removeActionGroup(this, ActionGroup.DISTANCE_VERY_CLOSE);
        skipPlayerDistanceCheck = true;
      } else {
        ActionData actionData = this.getActionData(ActionType.ON_DISTANCE_VERY_CLOSE);
        for (Player player : listOfPlayers) {
          if (player instanceof ServerPlayer serverPlayer && !ActionManager.containsPlayer(this,
              ActionGroup.DISTANCE_VERY_CLOSE, serverPlayer)) {
            this.executeAction(actionData, serverPlayer);
            ActionManager.addPlayer(this, ActionGroup.DISTANCE_VERY_CLOSE, serverPlayer);
          }
        }
      }
    }

    // Touch distance action, if set.
    if (this.hasAction(ActionType.ON_DISTANCE_TOUCH)) {
      List<Player> listOfPlayers = skipPlayerDistanceCheck ? null : this.getPlayersInRange(1.25D);
      if (listOfPlayers == null || listOfPlayers.isEmpty()) {
        ActionManager.removeActionGroup(this, ActionGroup.DISTANCE_TOUCH);
        skipPlayerDistanceCheck = true;
      } else {
        ActionData actionData = this.getActionData(ActionType.ON_DISTANCE_TOUCH);
        for (Player player : listOfPlayers) {
          if (player instanceof ServerPlayer serverPlayer
              && !ActionManager.containsPlayer(this, ActionGroup.DISTANCE_TOUCH, serverPlayer)) {
            this.executeAction(actionData, serverPlayer);
            ActionManager.addPlayer(this, ActionGroup.DISTANCE_TOUCH, serverPlayer);
          }
        }
      }
    }

    this.level.getProfiler().pop();
  }

  public InteractionResult openTradingScreen(ServerPlayer serverPlayer) {
    if (!this.level.isClientSide) {
      log.debug("Open trading screen for {} ... with {}", this, getOffers());
      this.setTradingPlayer(serverPlayer);
      this.openTradingScreen(serverPlayer,
          getCustomName() != null ? getCustomName()
              : Component.translatable(Constants.TEXT_PREFIX + "trading"),
          BASE_TICKS_REQUIRED_TO_FREEZE);
    }
    return InteractionResult.sidedSuccess(this.level.isClientSide);
  }

  @Override
  public void baseTick() {
    if (!this.level.isClientSide && this.isAlive() && this.baseTicker++ >= BASE_TICK) {
      npcBaseTick();
      this.baseTicker = 0;
    }
    super.baseTick();
  }

  @Override
  public boolean isAttackable() {
    return false;
  }

  @Override
  public boolean isPushable() {
    return false;
  }

  @Override
  public boolean removeWhenFarAway(double distance) {
    return false;
  }

  @Override
  protected void registerGoals() {
    super.registerGoals();
    this.goalSelector.addGoal(9, new ResetLookAtPlayerGoal(this));
    this.goalSelector.addGoal(9, new CustomLookAtPlayerGoal(this, Player.class, 15.0F, 1.0F));
    this.goalSelector.addGoal(10, new CustomLookAtPlayerGoal(this, Mob.class, 15.0F));
  }

  @Override
  public void travel(Vec3 vec3) {

    // Handle movement for NPC for specific conditions.
    if (travelTicker++ >= TRAVEL_TICK) {

      // Define if NPC is on ground or not.
      BlockState blockState = this.level.getBlockState(this.getOnPos());
      this.setOnGround(!blockState.is(Blocks.AIR) && !blockState.is(Blocks.GRASS)
          && !blockState.is(Blocks.WHITE_CARPET) && !blockState.is(Blocks.RED_CARPET));

      // Allow movement for NPC, if freefall is enabled.
      if (this.getAttributeFreefall() && !this.onGround) {
        this.setPos(this.getX(), Math.floor(this.getY() - 0.1d), this.getZ());
      }
      travelTicker = 0;
    }

    // Make sure we only calculate animations for be as much as possible server-friendly.
    this.calculateEntityAnimation(this instanceof FlyingAnimal);
  }

  @Override
  @Nullable
  public SpawnGroupData finalizeSpawn(ServerLevelAccessor serverLevelAccessor,
      DifficultyInstance difficulty, MobSpawnType mobSpawnType,
      @Nullable SpawnGroupData spawnGroupData, @Nullable CompoundTag compoundTag) {
    spawnGroupData = super.finalizeSpawn(serverLevelAccessor, difficulty, mobSpawnType,
        spawnGroupData, compoundTag);

    finalizeSpawn();
    return spawnGroupData;
  }

  @Override
  public InteractionResult mobInteract(Player player, InteractionHand hand) {
    if (player instanceof ServerPlayer serverPlayer && hand == InteractionHand.MAIN_HAND) {

      // Open configuration menu for EasyNPC wand item in hand.
      ItemStack handItem = player.getItemInHand(hand);
      if (!handItem.isEmpty() && handItem.getItem() == ModItems.EASY_NPC_WAND.get()) {
        this.openMainConfigurationMenu(serverPlayer);
        return InteractionResult.PASS;
      }

      // Pre-check for better auto handling of interaction.
      boolean hasInteractionAction = this.hasAction(ActionType.ON_INTERACTION);
      boolean hasSimpleDialog = this.hasSimpleDialog();
      boolean hasTrading = this.hasTrading();

      // Open configuration menu for creative mode and owner if no dialog or
      // interaction action is set or the player is crouching.
      if ((player.isCreative() || this.isOwner(serverPlayer))
          && ((!hasSimpleDialog && !hasInteractionAction && !hasTrading) || player.isCrouching())) {
        this.openMainConfigurationMenu(serverPlayer);
        return InteractionResult.PASS;
      }

      // Execute interaction action, if set.
      if (hasInteractionAction) {
        ActionData actionData = this.getActionData(ActionType.ON_INTERACTION);
        this.executeAction(actionData, serverPlayer);
      }

      // Open dialog menu, if we have a simple dialog.
      if (hasSimpleDialog) {
        EasyNPCEntityMenu.openDialogMenu(serverPlayer, this);
        return InteractionResult.CONSUME;
      }

      // Open trading screen, if we have a trading inventory.
      if (hasTrading) {
        this.openTradingScreen(serverPlayer);
        return InteractionResult.CONSUME;
      }

    }

    return InteractionResult.PASS;
  }

}
