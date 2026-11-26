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

import de.markusbordihn.easynpc.data.action.UniqueActionIdAssigner;
import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.FactionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.InventoryDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ProgressionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ServerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StateDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TickerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttributeHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.BaseTickHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.PendingActionHandler;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.Saddleable;
import net.minecraft.world.entity.SpawnGroupData;

public interface EasyNPCBase<E extends Mob>
    extends Saddleable,
        EasyNPC<E>,
        ActionEventDataCapable<E>,
        ActionHandler<E>,
        AttackDataCapable<E>,
        AttributeDataCapable<E>,
        AttributeHandler<E>,
        BaseTickHandler<E>,
        ConfigDataCapable<E>,
        ConfigurationDataCapable<E>,
        DialogDataCapable<E>,
        DisplayAttributeDataCapable<E>,
        FactionDataCapable<E>,
        InventoryDataCapable<E>,
        ModelDataCapable<E>,
        NavigationDataCapable<E>,
        ObjectiveDataCapable<E>,
        OwnerDataCapable<E>,
        PendingActionHandler<E>,
        PresetDataCapable<E>,
        ProfessionDataCapable<E>,
        ProgressionDataCapable<E>,
        RenderDataCapable<E>,
        ServerDataCapable<E>,
        SkinDataCapable<E>,
        SoundDataCapable<E>,
        StateDataCapable<E>,
        StatusDataCapable<E>,
        TickerDataCapable<E>,
        TradingDataCapable<E>,
        VariantDataCapable<E> {

  @Override
  default <T> void setSynchedEntityData(SynchedDataIndex synchedDataIndex, T data) {
    if (synchedDataIndex.persistent) {
      StatusDataCapable<E> statusData = this.getEasyNPCStatusData();
      if (statusData != null) {
        statusData.markNPCDataUpdated(synchedDataIndex);
      }
    }
    this.setSynchedEntityData(synchedDataIndex, data, false);
  }

  default void registerEasyNPCDefaultVariant(Enum<?> variant) {
    log.debug("Register default variant for {} with variant {} ...", this, variant);
    VariantDataCapable<E> variantData = this.getEasyNPCVariantData();
    if (variantData != null) {
      if (variantData.getSkinVariantType() == variant) {
        variantData.handleSkinVariantTypeChange(variant);
      } else {
        variantData.setSkinVariantType(variant);
      }
    }
    SoundDataCapable<E> soundData = this.getEasyNPCSoundData();
    if (soundData != null) {
      soundData.registerDefaultSoundData(variant);
    }
  }

  default SpawnGroupData getVanillaSpawnGroupData(SpawnGroupData spawnGroupData) {
    return spawnGroupData;
  }

  default SpawnGroupData finalizeEasyNPCSpawn(SpawnGroupData spawnGroupData) {
    log.debug("Finalize spawn for {} ...", this);

    NavigationDataCapable<?> navigationData = this.getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.setHomePositionIfMissing(this.getEntity().blockPosition());
    }

    StatusDataCapable<?> statusData = this.getEasyNPCStatusData();
    if (statusData == null || !statusData.getStatusDataFlag(StatusDataType.FINALIZED)) {
      this.registerEasyNPCDefaultData();
    } else {
      log.debug("Skip default data registration for {} ...", this);
    }

    return spawnGroupData;
  }

  default void defineEasyNPCBaseSyncedData() {
    // First define variant data to ensure that all other data can be linked to the variant.
    this.ifPresent(this.getEasyNPCVariantData(), VariantDataCapable::defineSynchedVariantData);

    this.ifPresent(
        this.getEasyNPCActionEventData(), ActionEventDataCapable::defineSynchedActionData);
    this.ifPresent(this.getEasyNPCAttackData(), AttackDataCapable::defineSynchedAttackData);
    this.ifPresent(
        this.getEasyNPCAttributeData(), AttributeDataCapable::defineSynchedAttributeData);
    this.ifPresent(this.getEasyNPCDialogData(), DialogDataCapable::defineSynchedDialogData);
    DisplayAttributeDataCapable<E> displayAttributeData = this.getEasyNPCDisplayAttributeData();
    if (displayAttributeData != null) {
      displayAttributeData.defineSynchedDisplayAttributeData();
      displayAttributeData.defineSynchedVisibilityConditionData();
    }
    this.ifPresent(this.getEasyNPCModelData(), ModelDataCapable::defineSynchedModelData);
    this.ifPresent(
        this.getEasyNPCNavigationData(), NavigationDataCapable::defineSynchedNavigationData);
    this.ifPresent(this.getEasyNPCOwnerData(), OwnerDataCapable::defineSynchedOwnerData);
    this.ifPresent(
        this.getEasyNPCProfessionData(), ProfessionDataCapable::defineSynchedProfessionData);
    this.ifPresent(
        this.getEasyNPCProgressionData(), ProgressionDataCapable::defineSynchedProgressionData);
    this.ifPresent(this.getEasyNPCRenderData(), RenderDataCapable::defineSynchedRenderData);
    this.ifPresent(this.getEasyNPCSkinData(), SkinDataCapable::defineSynchedSkinData);
    this.ifPresent(this.getEasyNPCSoundData(), SoundDataCapable::defineSynchedSoundData);
    this.ifPresent(this.getEasyNPCStatusData(), StatusDataCapable::defineSynchedStatusData);
    this.ifPresent(this.getEasyNPCTradingData(), TradingDataCapable::defineSynchedTradingData);
  }

  default void defineEasyNPCBaseServerSideData() {
    if (!this.isServerSideInstance()) {
      return;
    }

    ServerDataCapable<E> serverData = this.getEasyNPCServerData();
    if (serverData == null) {
      log.error("No server data available for {}", this.getEntityUUID());
      return;
    }

    if (!serverData.hasServerEntityData()) {
      serverData.defineServerEntityData();
    }

    this.ifPresent(
        this.getEasyNPCActionEventData(), ActionEventDataCapable::defineCustomActionData);
    this.ifPresent(this.getEasyNPCAttackData(), AttackDataCapable::defineCustomAttackData);
    this.ifPresent(this.getEasyNPCDialogData(), DialogDataCapable::defineCustomDialogData);
    this.ifPresent(
        this.getEasyNPCDisplayAttributeData(),
        DisplayAttributeDataCapable::defineCustomVisibilityConditionData);
    this.ifPresent(this.getEasyNPCObjectiveData(), ObjectiveDataCapable::defineCustomObjectiveData);
    this.ifPresent(this.getEasyNPCStateData(), StateDataCapable::defineCustomStateData);
    this.ifPresent(this.getEasyNPCPresetData(), PresetDataCapable::defineCustomPresetData);
    this.ifPresent(this.getEasyNPCFactionData(), FactionDataCapable::defineCustomFactionData);
  }

  default void handleEasyNPCData(String dataName, Runnable dataOperation) {
    try {
      dataOperation.run();
    } catch (Exception exception) {
      log.error(
          "Failed to handle {} data for {} ({}), using defaults instead!",
          dataName,
          this,
          this.getEntityUUID(),
          exception);
    }
  }

  default void addEasyNPCBaseAdditionalSaveData(CompoundTag compoundTag) {
    this.handleEasyNPCData(
        "action event",
        this.getEasyNPCActionEventData(),
        ActionEventDataCapable::addAdditionalActionData,
        compoundTag);
    this.handleEasyNPCData(
        "attack",
        this.getEasyNPCAttackData(),
        AttackDataCapable::addAdditionalAttackData,
        compoundTag);
    this.handleEasyNPCData(
        "attribute",
        this.getEasyNPCAttributeData(),
        AttributeDataCapable::addAdditionalAttributeData,
        compoundTag);
    this.handleEasyNPCData(
        "config",
        this.getEasyNPCConfigData(),
        ConfigDataCapable::addAdditionalConfigData,
        compoundTag);
    this.handleEasyNPCData(
        "dialog",
        this.getEasyNPCDialogData(),
        DialogDataCapable::addAdditionalDialogData,
        compoundTag);
    this.handleEasyNPCData(
        "display attribute",
        this.getEasyNPCDisplayAttributeData(),
        DisplayAttributeDataCapable::addAdditionalDisplayAttributeData,
        compoundTag);
    this.handleEasyNPCData(
        "faction",
        this.getEasyNPCFactionData(),
        FactionDataCapable::addAdditionalFactionData,
        compoundTag);
    this.handleEasyNPCData(
        "inventory",
        this.getEasyNPCInventoryData(),
        InventoryDataCapable::addAdditionalInventoryData,
        compoundTag);
    this.handleEasyNPCData(
        "model", this.getEasyNPCModelData(), ModelDataCapable::addAdditionalModelData, compoundTag);
    this.handleEasyNPCData(
        "navigation",
        this.getEasyNPCNavigationData(),
        NavigationDataCapable::addAdditionalNavigationData,
        compoundTag);
    this.handleEasyNPCData(
        "objective",
        this.getEasyNPCObjectiveData(),
        ObjectiveDataCapable::addAdditionalObjectiveData,
        compoundTag);
    this.handleEasyNPCData(
        "owner", this.getEasyNPCOwnerData(), OwnerDataCapable::addAdditionalOwnerData, compoundTag);
    this.handleEasyNPCData(
        "preset",
        this.getEasyNPCPresetData(),
        PresetDataCapable::addAdditionalPresetData,
        compoundTag);
    this.handleEasyNPCData(
        "profession",
        this.getEasyNPCProfessionData(),
        ProfessionDataCapable::addAdditionalProfessionData,
        compoundTag);
    this.handleEasyNPCData(
        "progression",
        this.getEasyNPCProgressionData(),
        ProgressionDataCapable::addAdditionalProgressionData,
        compoundTag);
    this.handleEasyNPCData(
        "render",
        this.getEasyNPCRenderData(),
        RenderDataCapable::addAdditionalRenderData,
        compoundTag);
    this.handleEasyNPCData(
        "skin", this.getEasyNPCSkinData(), SkinDataCapable::addAdditionalSkinData, compoundTag);
    this.handleEasyNPCData(
        "sound", this.getEasyNPCSoundData(), SoundDataCapable::addAdditionalSoundData, compoundTag);
    this.handleEasyNPCData(
        "state", this.getEasyNPCStateData(), StateDataCapable::addAdditionalStateData, compoundTag);
    this.handleEasyNPCData(
        "status",
        this.getEasyNPCStatusData(),
        StatusDataCapable::addAdditionalStatusData,
        compoundTag);
    this.handleEasyNPCData(
        "trading",
        this.getEasyNPCTradingData(),
        TradingDataCapable::addAdditionalTradingData,
        compoundTag);
    this.handleEasyNPCData(
        "variant",
        this.getEasyNPCVariantData(),
        VariantDataCapable::addAdditionalVariantData,
        compoundTag);
  }

  default void readEasyNPCBaseAdditionalSaveData(CompoundTag compoundTag) {
    // First read important data to ensure that all other data can be linked to the variant.
    this.handleEasyNPCData(
        "config",
        this.getEasyNPCConfigData(),
        ConfigDataCapable::readAdditionalConfigData,
        compoundTag);
    this.handleEasyNPCData(
        "variant",
        this.getEasyNPCVariantData(),
        VariantDataCapable::readAdditionalVariantData,
        compoundTag);

    ActionEventDataCapable<E> actionEventData = this.getEasyNPCActionEventData();
    this.handleEasyNPCData(
        "action event",
        actionEventData,
        ActionEventDataCapable::readAdditionalActionData,
        compoundTag);
    this.handleEasyNPCData(
        "attack",
        this.getEasyNPCAttackData(),
        AttackDataCapable::readAdditionalAttackData,
        compoundTag);
    this.handleEasyNPCData(
        "attribute",
        this.getEasyNPCAttributeData(),
        AttributeDataCapable::readAdditionalAttributeData,
        compoundTag);
    DialogDataCapable<E> dialogData = this.getEasyNPCDialogData();
    this.handleEasyNPCData(
        "dialog", dialogData, DialogDataCapable::readAdditionalDialogData, compoundTag);
    this.handleEasyNPCData(
        "display attribute",
        this.getEasyNPCDisplayAttributeData(),
        DisplayAttributeDataCapable::readAdditionalDisplayAttributeData,
        compoundTag);
    this.handleEasyNPCData(
        "faction",
        this.getEasyNPCFactionData(),
        FactionDataCapable::readAdditionalFactionData,
        compoundTag);
    this.handleEasyNPCData(
        "inventory",
        this.getEasyNPCInventoryData(),
        InventoryDataCapable::readAdditionalInventoryData,
        compoundTag);
    this.handleEasyNPCData(
        "model",
        this.getEasyNPCModelData(),
        ModelDataCapable::readAdditionalModelData,
        compoundTag);
    NavigationDataCapable<E> navigationData = this.getEasyNPCNavigationData();
    this.handleEasyNPCData(
        "navigation",
        navigationData,
        NavigationDataCapable::readAdditionalNavigationData,
        compoundTag);
    this.handleEasyNPCData(
        "owner",
        this.getEasyNPCOwnerData(),
        OwnerDataCapable::readAdditionalOwnerData,
        compoundTag);
    this.handleEasyNPCData(
        "preset",
        this.getEasyNPCPresetData(),
        PresetDataCapable::readAdditionalPresetData,
        compoundTag);
    this.handleEasyNPCData(
        "profession",
        this.getEasyNPCProfessionData(),
        ProfessionDataCapable::readAdditionalProfessionData,
        compoundTag);
    this.handleEasyNPCData(
        "progression",
        this.getEasyNPCProgressionData(),
        ProgressionDataCapable::readAdditionalProgressionData,
        compoundTag);
    this.handleEasyNPCData(
        "render",
        this.getEasyNPCRenderData(),
        RenderDataCapable::readAdditionalRenderData,
        compoundTag);
    this.handleEasyNPCData(
        "skin", this.getEasyNPCSkinData(), SkinDataCapable::readAdditionalSkinData, compoundTag);
    this.handleEasyNPCData(
        "sound",
        this.getEasyNPCSoundData(),
        SoundDataCapable::readAdditionalSoundData,
        compoundTag);
    this.handleEasyNPCData(
        "state",
        this.getEasyNPCStateData(),
        StateDataCapable::readAdditionalStateData,
        compoundTag);
    StatusDataCapable<E> statusData = this.getEasyNPCStatusData();
    this.handleEasyNPCData(
        "status", statusData, StatusDataCapable::readAdditionalStatusData, compoundTag);
    TradingDataCapable<E> tradingData = this.getEasyNPCTradingData();
    this.handleEasyNPCData(
        "trading", tradingData, TradingDataCapable::readAdditionalTradingData, compoundTag);
    this.handleEasyNPCData(
        "action ids",
        () ->
            UniqueActionIdAssigner.assign(
                dialogData != null ? dialogData.getDialogDataSet() : null,
                actionEventData != null ? actionEventData.getActionEventSet() : null,
                tradingData != null ? tradingData.getTradingDataSet() : null));

    this.handleEasyNPCData(
        "objective",
        this.getEasyNPCObjectiveData(),
        ObjectiveDataCapable::readAdditionalObjectiveData,
        compoundTag);

    this.handleEasyNPCData(
        "navigation refresh", navigationData, NavigationDataCapable::refreshNavigation);

    if (statusData != null) {
      boolean replacesRunningNPCData = this.isAddedToServerLevel();
      this.handleEasyNPCData(
          "status tracking",
          () -> statusData.trackLoadedNPCData(compoundTag, replacesRunningNPCData));
    }

    this.getMob().setPersistenceRequired();
  }

  private <D> void ifPresent(D data, Consumer<D> dataOperation) {
    if (data != null) {
      dataOperation.accept(data);
    }
  }

  private <D> void handleEasyNPCData(String dataName, D data, Consumer<D> dataOperation) {
    if (data != null) {
      this.handleEasyNPCData(dataName, () -> dataOperation.accept(data));
    }
  }

  private <D> void handleEasyNPCData(
      String dataName, D data, BiConsumer<D, CompoundTag> dataOperation, CompoundTag compoundTag) {
    if (data != null) {
      this.handleEasyNPCData(dataName, () -> dataOperation.accept(data, compoundTag));
    }
  }

  private boolean isAddedToServerLevel() {
    Entity entity = this.getEntity();
    return entity.level() instanceof ServerLevel serverLevel
        && serverLevel.getEntity(entity.getId()) == entity;
  }
}
