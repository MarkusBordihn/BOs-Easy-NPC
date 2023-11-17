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

package de.markusbordihn.easynpc.entity;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.commands.CommandManager;
import de.markusbordihn.easynpc.data.action.ActionData;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.action.ActionGroup;
import de.markusbordihn.easynpc.data.action.ActionManager;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.item.ModItems;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import javax.annotation.Nullable;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.DifficultyInstance;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.entity.SpawnGroupData;
import net.minecraft.world.entity.ai.navigation.GroundPathNavigation;
import net.minecraft.world.entity.animal.FlyingAnimal;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.ServerLevelAccessor;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.Vec3;

public class EasyNPCEntity extends EasyNPCEntityData {

  // Shared constants
  public static final MobCategory CATEGORY = MobCategory.MISC;
  // Additional ticker
  private static final int BASE_TICK = 16;
  private static final int DELAYED_REGISTRATION_TICK = 20 * 15; // 15 seconds
  private static final int OBJECTIVE_TICK = 40;
  private static final int TRADING_TICK = Math.round((20f / BASE_TICK) * 60) - 10;
  private static final int TRAVEL_TICK = 20;
  private int baseTicker = random.nextInt(BASE_TICK / 2);
  private int delayedRegistrationTicker = random.nextInt(DELAYED_REGISTRATION_TICK / 2);
  private int objectiveTicker = random.nextInt(OBJECTIVE_TICK / 2);
  private int resetTradingTicker = 0;
  private int tradingTicker = random.nextInt(TRADING_TICK / 2);
  private int travelTicker = random.nextInt(TRAVEL_TICK / 2);

  public EasyNPCEntity(
      EntityType<? extends EasyNPCEntity> entityType, Level level, Enum<?> variant) {
    this(entityType, level);
    this.setVariant(variant);

    // Distribute Ticks along several entities.
    this.baseTicker = this.random.nextInt(0, BASE_TICK / 2);
  }

  public EasyNPCEntity(EntityType<? extends EasyNPCEntity> entityType, Level level) {
    super(entityType, level);
    this.setInvulnerable(true);
    this.refreshGroundNavigation();
  }

  public void finalizeSpawn() {
    // Do stuff like default names.
  }

  public void executeActions(Set<ActionData> actionDataSet, ServerPlayer serverPlayer) {
    if (actionDataSet == null || actionDataSet.isEmpty()) {
      return;
    }
    for (ActionData actionData : actionDataSet) {
      this.executeAction(actionData, serverPlayer);
    }
  }

  public void executeAction(ActionData actionData, ServerPlayer serverPlayer) {
    if (actionData == null || !actionData.isValidAndNotEmpty() || this.level.isClientSide) {
      return;
    }

    switch (actionData.getType()) {
      case NONE:
        break;
      case COMMAND:
        if (actionData.shouldExecuteAsUser()) {
          // Check if we have a valid permission level based on the user permission level.
          int userPermissionLevel = actionData.getPermissionLevel();
          if (userPermissionLevel > getActionPermissionLevel()) {
            log.warn(
                "User permission level {} is lower than action permission level {} for action {}",
                getActionPermissionLevel(),
                userPermissionLevel,
                actionData);
            userPermissionLevel = getActionPermissionLevel();
          }

          // Execute action as user with define permission level (default: 1).
          log.debug(
              "Try to execute action {} as user {} with action permission level {} ...",
              actionData,
              serverPlayer,
              actionData.getPermissionLevel());
          CommandManager.executePlayerCommand(
              actionData.getAction(this, serverPlayer),
              serverPlayer,
              actionData.getPermissionLevel(),
              actionData.isDebugEnabled());
        } else {
          // Make sure permission level is not too low or to high.
          int ownerPermissionLevel = getActionPermissionLevel();
          if (ownerPermissionLevel > 3) {
            ownerPermissionLevel = 3;
          } else if (ownerPermissionLevel <= 0) {
            ownerPermissionLevel = 1;
          }

          // Execute action as NPC entity with owner permission level.
          log.debug(
              "Try to execute action {} as entity {} with owner permission level {} of max. {} ...",
              actionData,
              this,
              ownerPermissionLevel,
              getActionPermissionLevel());
          CommandManager.executeEntityCommand(
              actionData.getAction(this, serverPlayer),
              this,
              ownerPermissionLevel,
              actionData.isDebugEnabled());
        }
        break;
      case OPEN_NAMED_DIALOG:
        String dialogLabel = actionData.getCommand();
        if (dialogLabel != null && !dialogLabel.isEmpty() && this.hasDialog(dialogLabel)) {
          UUID dialogId = this.getDialogId(dialogLabel);
          EasyNPCEntityMenu.openDialogMenu(serverPlayer, this, dialogId, 0);
        } else {
          log.error("Unknown dialog label {} for action {}", dialogLabel, actionData);
        }
        break;
      case OPEN_TRADING_SCREEN:
        log.debug("Execute open trading screen for {} from {}", this, serverPlayer);
        this.openTradingScreen(serverPlayer);
        break;
      default:
        log.warn("Unknown action type {} for action {}", actionData.getType(), actionData);
        break;
    }
  }

  public void openMainConfigurationMenu(ServerPlayer serverPlayer) {
    EasyNPCEntityMenu.openMainConfigurationMenu(serverPlayer, this);
  }

  public void npcBaseTick() {
    this.level.getProfiler().push("npcBaseTick");

    // Check distance for additional actions.
    checkDistanceActions();

    // Check if we have a trading inventory and update it.
    if (this.tradingTicker++ >= TRADING_TICK) {
      if (this.hasTrading()) {
        checkTradingActions();
      }
      this.tradingTicker = 0;
    }

    this.level.getProfiler().pop();
  }

  public void npcObjectiveTick() {
    this.level.getProfiler().push("npcObjectiveTick");

    this.level.getProfiler().pop();
  }

  public void checkTradingActions() {
    this.level.getProfiler().push("npcCheckTradingActions");

    // Check if we have a trading inventory which needs a reset.
    if ((this.getTradingType() == TradingType.BASIC
            || this.getTradingType() == TradingType.ADVANCED)
        && this.getTradingResetsEveryMin() > 0
        && this.resetTradingTicker++ >= this.getTradingResetsEveryMin()) {
      this.resetTradingOffers();
      this.resetTradingTicker = 0;
    }

    this.level.getProfiler().pop();
  }

  public void checkDistanceActions() {
    this.level.getProfiler().push("npcCheckDistanceActions");
    // Check to avoid additional checks, when no player is in range.
    boolean skipPlayerDistanceCheck = false;

    // Near distance action, if set.
    if (this.hasActionEvent(ActionEventType.ON_DISTANCE_NEAR)) {
      List<Player> listOfPlayers = this.getPlayersInRange(16.0D);
      if (listOfPlayers == null || listOfPlayers.isEmpty()) {
        ActionManager.removeActionGroup(this, ActionGroup.DISTANCE_NEAR);
        skipPlayerDistanceCheck = true;
      } else {
        ActionData actionData = this.getActionEvent(ActionEventType.ON_DISTANCE_NEAR);
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
    if (this.hasActionEvent(ActionEventType.ON_DISTANCE_CLOSE)) {
      List<Player> listOfPlayers = skipPlayerDistanceCheck ? null : this.getPlayersInRange(8.0D);
      if (listOfPlayers == null || listOfPlayers.isEmpty()) {
        ActionManager.removeActionGroup(this, ActionGroup.DISTANCE_CLOSE);
        skipPlayerDistanceCheck = true;
      } else {
        ActionData actionData = this.getActionEvent(ActionEventType.ON_DISTANCE_CLOSE);
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
    if (this.hasActionEvent(ActionEventType.ON_DISTANCE_VERY_CLOSE)) {
      List<Player> listOfPlayers = skipPlayerDistanceCheck ? null : this.getPlayersInRange(4.0D);
      if (listOfPlayers == null || listOfPlayers.isEmpty()) {
        ActionManager.removeActionGroup(this, ActionGroup.DISTANCE_VERY_CLOSE);
        skipPlayerDistanceCheck = true;
      } else {
        ActionData actionData = this.getActionEvent(ActionEventType.ON_DISTANCE_VERY_CLOSE);
        for (Player player : listOfPlayers) {
          if (player instanceof ServerPlayer serverPlayer
              && !ActionManager.containsPlayer(
                  this, ActionGroup.DISTANCE_VERY_CLOSE, serverPlayer)) {
            this.executeAction(actionData, serverPlayer);
            ActionManager.addPlayer(this, ActionGroup.DISTANCE_VERY_CLOSE, serverPlayer);
          }
        }
      }
    }

    // Touch distance action, if set.
    if (this.hasActionEvent(ActionEventType.ON_DISTANCE_TOUCH)) {
      List<Player> listOfPlayers = skipPlayerDistanceCheck ? null : this.getPlayersInRange(1.25D);
      if (listOfPlayers == null || listOfPlayers.isEmpty()) {
        ActionManager.removeActionGroup(this, ActionGroup.DISTANCE_TOUCH);
        skipPlayerDistanceCheck = true;
      } else {
        ActionData actionData = this.getActionEvent(ActionEventType.ON_DISTANCE_TOUCH);
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
      log.debug("Open trading screen for {} with {}", this, getOffers());
      this.setTradingPlayer(serverPlayer);
      this.openTradingScreen(
          serverPlayer,
          getCustomName() != null
              ? getCustomName()
              : Component.translatable(Constants.TEXT_PREFIX + "trading"),
          BASE_TICKS_REQUIRED_TO_FREEZE);
    }
    return InteractionResult.sidedSuccess(this.level.isClientSide);
  }

  public void onEasyNPCJoin(EasyNPCEntity easyNPCEntity) {
    this.onEasyNPCJoinUpdateObjective(easyNPCEntity);
  }

  public void onEasyNPCLeave(EasyNPCEntity easyNPCEntity) {
    this.onEasyNPCLeaveUpdateObjective(easyNPCEntity);
  }

  public void onPlayerJoin(ServerPlayer serverPlayer) {
    this.onPlayerJoinUpdateObjective(serverPlayer);
  }

  public void onPlayerLeave(ServerPlayer serverPlayer) {
    this.onPlayerLeaveUpdateObjective(serverPlayer);
  }

  public void onLivingEntityJoin(LivingEntity livingEntity) {
    this.onLivingEntityJoinUpdateObjective(livingEntity);
  }

  public void onLivingEntityLeave(LivingEntity livingEntity) {
    this.onLivingEntityLeaveUpdateObjective(livingEntity);
  }

  public void refreshGroundNavigation() {
    if (this.navigation instanceof GroundPathNavigation groundPathNavigation) {
      if (this.synchedDataLoaded()) {
        groundPathNavigation.setCanOpenDoors(getAttributeCanOpenDoor());
        groundPathNavigation.setCanPassDoors(getAttributeCanPassDoor());
        groundPathNavigation.setCanFloat(getAttributeCanFloat());
      } else {
        groundPathNavigation.setCanOpenDoors(true);
        groundPathNavigation.setCanPassDoors(true);
        groundPathNavigation.setCanFloat(true);
      }
    }
  }

  @Override
  public boolean doHurtTarget(Entity entity) {
    boolean hurtResult = super.doHurtTarget(entity);
    this.attackAnimationTick = 10;
    return hurtResult;
  }

  @Override
  public void handleEntityEvent(byte flag) {
    super.handleEntityEvent(flag);
    if (flag == 4) {
      this.attackAnimationTick = 10;
    }
  }

  @Override
  public void aiStep() {
    super.aiStep();
    if (this.attackAnimationTick > 0) {
      --this.attackAnimationTick;
    }

    if (!this.level.isClientSide) {
      this.updatePersistentAnger((ServerLevel) this.level, true);
    }
  }

  @Override
  public void baseTick() {
    super.baseTick();

    // Early exit for client side and dead entities.
    if (this.level.isClientSide || !this.isAlive()) {
      return;
    }

    // Delayed registration for EasyNPCs
    if (this.delayedRegistrationTicker++ >= DELAYED_REGISTRATION_TICK) {
      if (this.hasObjectives()) {
        this.refreshCustomObjectives();
      }
      this.delayedRegistrationTicker = 0;
    }

    // NPC Base Ticker (every 16 ticks)
    if (this.baseTicker++ >= BASE_TICK) {
      this.npcBaseTick();
      this.baseTicker = 0;
    }

    // NPC Objective Ticker (ever 40 ticks)
    if (this.objectiveTicker++ >= OBJECTIVE_TICK) {
      this.npcObjectiveTick();
      this.objectiveTicker = 0;
    }
  }

  @Override
  public boolean isAttackable() {
    return this.synchedDataLoaded() && getAttributeIsAttackable();
  }

  @Override
  public boolean isPushable() {
    return this.synchedDataLoaded() && getAttributeIsPushable();
  }

  @Override
  public boolean removeWhenFarAway(double distance) {
    return false;
  }

  @Override
  protected void registerGoals() {
    super.registerGoals();
    this.registerStandardObjectives();
  }

  @Override
  public void travel(Vec3 vec3) {

    // Update basic movement relevant data.
    if (travelTicker++ >= TRAVEL_TICK) {

      // Define if NPC is on ground or not.
      BlockState blockState = this.level.getBlockState(this.getOnPos());
      this.setOnGround(
          !blockState.is(Blocks.AIR)
              && !blockState.is(Blocks.GRASS)
              && !blockState.is(Blocks.WHITE_CARPET)
              && !blockState.is(Blocks.RED_CARPET));

      // Allow movement for NPC, if freefall is enabled.
      if (this.getAttributeFreefall() && !this.onGround) {
        this.setPos(this.getX(), Math.floor(this.getY() - 0.1d), this.getZ());
      }
      travelTicker = 0;
    }

    // Handle movement for NPC for specific conditions.
    if (this.hasObjectives()) {
      // Allow travel for NPC, if objectives are used.
      super.travel(vec3);
    } else {
      // Make sure we only calculate animations for be as much as possible server-friendly.
      this.calculateEntityAnimation(this, this instanceof FlyingAnimal);
    }
  }

  @Override
  @Nullable
  public SpawnGroupData finalizeSpawn(
      ServerLevelAccessor serverLevelAccessor,
      DifficultyInstance difficulty,
      MobSpawnType mobSpawnType,
      @Nullable SpawnGroupData spawnGroupData,
      @Nullable CompoundTag compoundTag) {
    spawnGroupData =
        super.finalizeSpawn(
            serverLevelAccessor, difficulty, mobSpawnType, spawnGroupData, compoundTag);
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
      boolean hasInteractionAction = this.hasActionEvent(ActionEventType.ON_INTERACTION);
      boolean hasDialog = this.hasDialog();
      boolean hasTrading = this.hasTrading();

      // Open configuration menu for creative mode and owner if no dialog or
      // interaction action is set or the player is crouching.
      if ((player.isCreative() || this.isOwner(serverPlayer))
          && ((!hasDialog && !hasInteractionAction && !hasTrading) || player.isCrouching())) {
        this.openMainConfigurationMenu(serverPlayer);
        return InteractionResult.PASS;
      }

      // Execute interaction action, if set.
      if (hasInteractionAction) {
        ActionData actionData = this.getActionEvent(ActionEventType.ON_INTERACTION);
        this.executeAction(actionData, serverPlayer);
      }

      // Open dialog menu, if we have a simple dialog.
      if (hasDialog) {
        UUID dialogId = this.getDialogDataSet().getDefaultDialogId();
        if (dialogId != null) {
          EasyNPCEntityMenu.openDialogMenu(serverPlayer, this, dialogId, 0);
          return InteractionResult.CONSUME;
        } else {
          log.error("Unable to get default dialog id for {}", this);
        }
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

  @Override
  public boolean hurt(DamageSource damageSource, float damage) {
    boolean damageResult = super.hurt(damageSource, damage);

    // Check for action event on hurt.
    if (damageResult && this.hasActionEvent(ActionEventType.ON_HURT)) {
      ActionData actionData = this.getActionEvent(ActionEventType.ON_HURT);
      if (actionData != null && actionData.isValidAndNotEmpty()) {
        this.executeAction(actionData, null);
      }
    }

    return damageResult;
  }

  @Override
  public void die(DamageSource damageSource) {
    // Check for action event on death.
    if (this.hasActionEvent(ActionEventType.ON_DEATH)) {
      ActionData actionData = this.getActionEvent(ActionEventType.ON_DEATH);
      if (actionData != null && actionData.isValidAndNotEmpty()) {
        this.executeAction(actionData, null);
      }
    }

    super.die(damageSource);
  }
}
