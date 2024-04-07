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

package de.markusbordihn.easynpc.entity.easynpc;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.custom.CustomDataAccessor;
import de.markusbordihn.easynpc.entity.EasyNPCBaseEntity;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigData;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationData;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.entity.easynpc.data.GuiData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationData;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveData;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetData;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionData;
import de.markusbordihn.easynpc.entity.easynpc.data.ScaleData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundData;
import de.markusbordihn.easynpc.entity.easynpc.data.SpawnData;
import de.markusbordihn.easynpc.entity.easynpc.data.SpawnerData;
import de.markusbordihn.easynpc.entity.easynpc.data.TickerData;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import java.util.Random;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.util.profiling.ProfilerFiller;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.NeutralMob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.control.LookControl;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import net.minecraft.world.entity.monster.CrossbowAttackMob;
import net.minecraft.world.entity.npc.Npc;
import net.minecraft.world.item.trading.Merchant;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface EasyNPC<E extends PathfinderMob> extends Npc {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  Random randomNumber = new Random();

  static Class<? extends Entity> getSynchedEntityDataClass() {
    return EasyNPCBaseEntity.class;
  }

  E getEasyNPCEntity();

  int getNPCDataVersion();

  void setNPCDataVersion(int version);

  default LookControl getLookControl() {
    return this instanceof Mob mob ? mob.getLookControl() : null;
  }

  default ActionEventData<E> getEasyNPCActionEventData() {
    return this instanceof ActionEventData<E> actionEventData ? actionEventData : null;
  }

  default AttackData<E> getEasyNPCAttackData() {
    return this instanceof AttackData<E> attackData ? attackData : null;
  }

  default AttributeData<E> getEasyNPCAttributeData() {
    return this instanceof AttributeData<E> attributeData ? attributeData : null;
  }

  default ConfigData<E> getEasyNPCConfigData() {
    return this instanceof ConfigData<E> configData ? configData : null;
  }

  default ConfigurationData<E> getEasyNPCConfigurationData() {
    return this instanceof ConfigurationData<E> configurationData ? configurationData : null;
  }

  default DialogData<E> getEasyNPCDialogData() {
    return this instanceof DialogData<E> dialogData ? dialogData : null;
  }

  default GuiData<E> getEasyNPCGuiData() {
    return this instanceof GuiData<E> guiData ? guiData : null;
  }

  default SkinData<E> getEasyNPCSkinData() {
    return this instanceof SkinData<E> skinData ? skinData : null;
  }

  default ModelData<E> getEasyNPCModelData() {
    return this instanceof ModelData<E> modelData ? modelData : null;
  }

  default NavigationData<E> getEasyNPCNavigationData() {
    return this instanceof NavigationData<E> navigationData ? navigationData : null;
  }

  default ObjectiveData<E> getEasyNPCObjectiveData() {
    return this instanceof ObjectiveData<E> objectiveData ? objectiveData : null;
  }

  default OwnerData<E> getEasyNPCOwnerData() {
    return this instanceof OwnerData<E> ownerData ? ownerData : null;
  }

  default PresetData<E> getEasyNPCPresetData() {
    return this instanceof PresetData<E> presetData ? presetData : null;
  }

  default ProfessionData<E> getEasyNPCProfessionData() {
    return this instanceof ProfessionData<E> professionData ? professionData : null;
  }

  default ScaleData<E> getEasyNPCScaleData() {
    return this instanceof ScaleData<E> scaleData ? scaleData : null;
  }

  default SpawnData<E> getEasyNPCSpawnData() {
    return this instanceof SpawnData<E> spawnData ? spawnData : null;
  }

  default SpawnerData<E> getEasyNPCSpawnerData() {
    return this instanceof SpawnerData<E> spawnerData ? spawnerData : null;
  }

  default TickerData<E> getEasyNPCTickerData() {
    return this instanceof TickerData<E> tickerData ? tickerData : null;
  }

  default TradingData<E> getEasyNPCTradingData() {
    return this instanceof TradingData<E> tradingData ? tradingData : null;
  }

  default SoundData<E> getEasyNPCSoundData() {
    return this instanceof SoundData<E> soundData ? soundData : null;
  }

  default VariantData<E> getEasyNPCVariantData() {
    return this instanceof VariantData<E> variantData ? variantData : null;
  }

  default ActionHandler<E> getEasyNPCActionHandler() {
    return this instanceof ActionHandler<E> actionHandler ? actionHandler : null;
  }

  default PathfinderMob getPathfinderMob() {
    return this instanceof PathfinderMob pathfinderMob ? pathfinderMob : null;
  }

  default Level getLevel() {
    return this instanceof Mob mob ? mob.level : null;
  }

  default ServerLevel getServerLevel() {
    return this instanceof Mob mob && mob.level instanceof ServerLevel serverLevel
        ? serverLevel
        : null;
  }

  default boolean isClientSide() {
    return this.getLevel() != null && this.getLevel().isClientSide();
  }

  default boolean isServerSide() {
    return !isClientSide();
  }

  default LivingEntity getLivingEntity() {
    return this instanceof LivingEntity livingEntity ? livingEntity : null;
  }

  default NeutralMob getNeutralMob() {
    return this instanceof NeutralMob neutralMob ? neutralMob : null;
  }

  default Merchant getMerchant() {
    return this instanceof Merchant merchant ? merchant : null;
  }

  default ProfilerFiller getProfiler() {
    return this instanceof Mob mob ? mob.level.getProfiler() : null;
  }

  default Entity getEntity() {
    return this instanceof Entity entity ? entity : null;
  }

  default Mob getMob() {
    return this instanceof Mob mob ? mob : null;
  }

  default UUID getUUID() {
    return this instanceof Entity entity ? entity.getUUID() : null;
  }

  GoalSelector getEntityGoalSelector();

  GoalSelector getEntityTargetSelector();

  default CrossbowAttackMob getCrossbowAttackMob() {
    return this instanceof CrossbowAttackMob crossbowAttackMob ? crossbowAttackMob : null;
  }

  default Component getEasyNPCTypeName() {
    return getEasyNPCEntity().getType().getDescription();
  }

  default void handlePlayerJoin(ServerPlayer serverPlayer) {
    ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.onPlayerJoinUpdateObjective(serverPlayer);
    }
  }

  default void handlePlayerLeave(ServerPlayer serverPlayer) {
    ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.onPlayerLeaveUpdateObjective(serverPlayer);
    }
  }

  default void handleLivingEntityJoin(LivingEntity livingEntity) {
    ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.onLivingEntityJoinUpdateObjective(livingEntity);
    }
  }

  default void handleLivingEntityLeave(LivingEntity livingEntity) {
    ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.onLivingEntityLeaveUpdateObjective(livingEntity);
    }
  }

  default void handleEasyNPCJoin(EasyNPC<?> entity) {
    ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null && entity != null) {
      objectiveData.onEasyNPCJoinUpdateObjective(entity);
    }
  }

  default void handleEasyNPCLeave(EasyNPC<?> entity) {
    ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null && entity != null) {
      objectiveData.onEasyNPCLeaveUpdateObjective(entity);
    }
  }

  default void handleDieEvent(DamageSource damageSource) {
    TradingData<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.stopTrading();
    }

    ActionEventData<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.handleActionDieEvent(damageSource);
    }
  }

  default void handleChangeDimensionEvent(ServerLevel serverLevel) {
    TradingData<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.stopTrading();
    }
  }

  default void handleHurtEvent(DamageSource damageSource, float damage) {
    ActionEventData<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.handleActionHurtEvent(damageSource, damage);
    }
  }

  default void registerEasyNPCDefaultHandler(Enum<?> variant) {
    log.info("Register default handler for {} with variant {} ...", this, variant);
    VariantData<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.setVariant(variant);
    }

    // Define server-side custom data.
    if (this.isServerSide()) {
      log.info("Define custom server-side data for {} ...", this);
      ActionEventData<E> actionEventData = getEasyNPCActionEventData();
      if (actionEventData != null) {
        actionEventData.defineCustomActionData();
      }
      DialogData<E> dialogData = getEasyNPCDialogData();
      if (dialogData != null) {
        dialogData.defineCustomDialogData();
      }
      ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
      if (objectiveData != null) {
        objectiveData.defineCustomObjectiveData();
      }
      SpawnerData<E> spawnerData = getEasyNPCSpawnerData();
      if (spawnerData != null) {
        spawnerData.defineCustomSpawnerData();
      }
    }

    // Register default data, if needed.
    AttributeData<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.registerDefaultAttributeData(variant);
    }
    SoundData<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.registerDefaultSoundData(variant);
    }
  }

  default void defineEasyNPCSyncedData() {
    log.info("Define synced data for {}", this);

    // First define variant data to ensure that all other data can be linked to the variant.
    VariantData<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.defineSynchedVariantData();
    }

    // Define all other synced data.
    ActionEventData<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.defineSynchedActionData();
    }
    AttackData<E> attackData = getEasyNPCAttackData();
    if (attackData != null) {
      attackData.defineSynchedAttackData();
    }
    AttributeData<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.defineSynchedAttributeData();
    }
    DialogData<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.defineSynchedDialogData();
    }
    ModelData<E> modelData = getEasyNPCModelData();
    if (modelData != null) {
      modelData.defineSynchedModelData();
    }
    NavigationData<E> navigationData = getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.defineSynchedNavigationData();
    }
    ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.defineSynchedObjectiveData();
    }
    OwnerData<E> ownerData = getEasyNPCOwnerData();
    if (ownerData != null) {
      ownerData.defineSynchedOwnerData();
    }
    ProfessionData<E> professionData = getEasyNPCProfessionData();
    if (professionData != null) {
      professionData.defineSynchedProfessionData();
    }
    ScaleData<E> scaleData = getEasyNPCScaleData();
    if (scaleData != null) {
      scaleData.defineSynchedScaleData();
    }
    SkinData<E> skinData = getEasyNPCSkinData();
    if (skinData != null) {
      skinData.defineSynchedSkinData();
    }
    SoundData<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.defineSynchedSoundData();
    }
    TradingData<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.defineSynchedTradingData();
    }
  }

  default void addEasyNPCAdditionalSaveData(CompoundTag compoundTag) {
    log.info("Add additional save data for {}", this);
    ActionEventData<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.addAdditionalActionData(compoundTag);
    }
    AttackData<E> attackData = getEasyNPCAttackData();
    if (attackData != null) {
      attackData.addAdditionalAttackData(compoundTag);
    }
    AttributeData<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.addAdditionalAttributeData(compoundTag);
    }
    ConfigData<E> configData = getEasyNPCConfigData();
    if (configData != null) {
      configData.addAdditionalConfigData(compoundTag);
    }
    DialogData<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.addAdditionalDialogData(compoundTag);
    }
    ModelData<E> modelData = getEasyNPCModelData();
    if (modelData != null) {
      modelData.addAdditionalModelData(compoundTag);
    }
    NavigationData<E> navigationData = getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.addAdditionalNavigationData(compoundTag);
    }
    ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.addAdditionalObjectiveData(compoundTag);
    }
    OwnerData<E> ownerData = getEasyNPCOwnerData();
    if (ownerData != null) {
      ownerData.addAdditionalOwnerData(compoundTag);
    }
    ProfessionData<E> professionData = getEasyNPCProfessionData();
    if (professionData != null) {
      professionData.addAdditionalProfessionData(compoundTag);
    }
    ScaleData<E> scaleData = getEasyNPCScaleData();
    if (scaleData != null) {
      scaleData.addAdditionalScaleData(compoundTag);
    }
    SkinData<E> skinData = getEasyNPCSkinData();
    if (skinData != null) {
      skinData.addAdditionalSkinData(compoundTag);
    }
    SoundData<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.addAdditionalSoundData(compoundTag);
    }
    SpawnerData<E> spawnerData = getEasyNPCSpawnerData();
    if (spawnerData != null) {
      spawnerData.addAdditionalSpawnerData(compoundTag);
    }
    TradingData<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.addAdditionalTradingData(compoundTag);
    }
    VariantData<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.addAdditionalVariantData(compoundTag);
    }
  }

  default void readEasyNPCAdditionalSaveData(CompoundTag compoundTag) {
    log.info("Read additional save data for {} ...", this);

    // First read important data to ensure that all other data can be linked to the variant.
    ConfigData<E> configData = getEasyNPCConfigData();
    if (configData != null) {
      configData.readAdditionalConfigData(compoundTag);
    }
    VariantData<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.readAdditionalVariantData(compoundTag);
    }

    // Read all other synced data.
    ActionEventData<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.readAdditionalActionData(compoundTag);
    }
    AttackData<E> attackData = getEasyNPCAttackData();
    if (attackData != null) {
      attackData.readAdditionalAttackData(compoundTag);
    }
    AttributeData<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.readAdditionalAttributeData(compoundTag);
    }
    DialogData<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.readAdditionalDialogData(compoundTag);
    }
    ModelData<E> modelData = getEasyNPCModelData();
    if (modelData != null) {
      modelData.readAdditionalModelData(compoundTag);
    }
    NavigationData<E> navigationData = getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.readAdditionalNavigationData(compoundTag);
    }
    ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.readAdditionalObjectiveData(compoundTag);
    }
    OwnerData<E> ownerData = getEasyNPCOwnerData();
    if (ownerData != null) {
      ownerData.readAdditionalOwnerData(compoundTag);
    }
    ProfessionData<E> professionData = getEasyNPCProfessionData();
    if (professionData != null) {
      professionData.readAdditionalProfessionData(compoundTag);
    }
    ScaleData<E> scaleData = getEasyNPCScaleData();
    if (scaleData != null) {
      scaleData.readAdditionalScaleData(compoundTag);
    }
    SkinData<E> skinData = getEasyNPCSkinData();
    if (skinData != null) {
      skinData.readAdditionalSkinData(compoundTag);
    }
    SoundData<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.readAdditionalSoundData(compoundTag);
    }
    SpawnerData<E> spawnerData = getEasyNPCSpawnerData();
    if (spawnerData != null) {
      spawnerData.readAdditionalSpawnerData(compoundTag);
    }
    TradingData<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.readAdditionalTradingData(compoundTag);
    }

    // Register Objectives after all data is loaded.
    if (objectiveData != null) {
      objectiveData.readAdditionalObjectiveData(compoundTag);
    }
  }

  <D> void setEasyNPCData(EntityDataAccessor<D> entityDataAccessor, D entityData);

  <D> D getEasyNPCData(EntityDataAccessor<D> entityDataAccessor);

  <D> void defineEasyNPCData(EntityDataAccessor<D> entityDataAccessor, D entityData);

  <D> void setEasyNPCCustomData(CustomDataAccessor<D> entityDataAccessor, D entityData);

  <D> D getEasyNPCCustomData(CustomDataAccessor<D> entityDataAccessor);

  <D> void defineEasyNPCCustomData(CustomDataAccessor<D> entityDataAccessor, D entityData);
}
