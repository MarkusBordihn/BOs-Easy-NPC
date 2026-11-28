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
import net.minecraft.core.HolderLookup;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.SpawnGroupData;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;

public interface EasyNPCBase<E extends Mob>
    extends EasyNPC<E>,
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
      navigationData.applyDefaultNPCHomePosition();
    }

    StatusDataCapable<?> statusData = this.getEasyNPCStatusData();
    if (statusData == null || !statusData.getStatusDataFlag(StatusDataType.FINALIZED)) {
      this.registerEasyNPCDefaultData();
    } else {
      log.debug("Skip default data registration for {} ...", this);
    }

    return spawnGroupData;
  }

  default void defineEasyNPCBaseSyncedData(SynchedEntityData.Builder builder) {
    // First define variant data to ensure that all other data can be linked to the variant.
    this.ifPresent(
        this.getEasyNPCVariantData(), VariantDataCapable::defineSynchedVariantData, builder);

    this.ifPresent(
        this.getEasyNPCActionEventData(), ActionEventDataCapable::defineSynchedActionData, builder);
    this.ifPresent(
        this.getEasyNPCAttackData(), AttackDataCapable::defineSynchedAttackData, builder);
    this.ifPresent(
        this.getEasyNPCAttributeData(), AttributeDataCapable::defineSynchedAttributeData, builder);
    this.ifPresent(
        this.getEasyNPCDialogData(), DialogDataCapable::defineSynchedDialogData, builder);
    DisplayAttributeDataCapable<E> displayAttributeData = this.getEasyNPCDisplayAttributeData();
    if (displayAttributeData != null) {
      displayAttributeData.defineSynchedDisplayAttributeData(builder);
      displayAttributeData.defineSynchedVisibilityConditionData(builder);
    }
    this.ifPresent(this.getEasyNPCModelData(), ModelDataCapable::defineSynchedModelData, builder);
    this.ifPresent(
        this.getEasyNPCNavigationData(),
        NavigationDataCapable::defineSynchedNavigationData,
        builder);
    this.ifPresent(this.getEasyNPCOwnerData(), OwnerDataCapable::defineSynchedOwnerData, builder);
    this.ifPresent(
        this.getEasyNPCProfessionData(),
        ProfessionDataCapable::defineSynchedProfessionData,
        builder);
    this.ifPresent(
        this.getEasyNPCProgressionData(),
        ProgressionDataCapable::defineSynchedProgressionData,
        builder);
    this.ifPresent(
        this.getEasyNPCRenderData(), RenderDataCapable::defineSynchedRenderData, builder);
    this.ifPresent(this.getEasyNPCSkinData(), SkinDataCapable::defineSynchedSkinData, builder);
    this.ifPresent(this.getEasyNPCSoundData(), SoundDataCapable::defineSynchedSoundData, builder);
    this.ifPresent(
        this.getEasyNPCStatusData(), StatusDataCapable::defineSynchedStatusData, builder);
    this.ifPresent(
        this.getEasyNPCTradingData(), TradingDataCapable::defineSynchedTradingData, builder);
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

  default void addEasyNPCBaseAdditionalSaveData(
      ValueOutput valueOutput, HolderLookup.Provider provider) {
    this.handleEasyNPCData(
        "action event",
        this.getEasyNPCActionEventData(),
        ActionEventDataCapable::addAdditionalActionData,
        valueOutput);
    this.handleEasyNPCData(
        "attack",
        this.getEasyNPCAttackData(),
        AttackDataCapable::addAdditionalAttackData,
        valueOutput);
    this.handleEasyNPCData(
        "attribute",
        this.getEasyNPCAttributeData(),
        AttributeDataCapable::addAdditionalAttributeData,
        valueOutput);
    this.handleEasyNPCData(
        "config",
        this.getEasyNPCConfigData(),
        ConfigDataCapable::addAdditionalConfigData,
        valueOutput);
    this.handleEasyNPCData(
        "dialog",
        this.getEasyNPCDialogData(),
        DialogDataCapable::addAdditionalDialogData,
        valueOutput);
    this.handleEasyNPCData(
        "display attribute",
        this.getEasyNPCDisplayAttributeData(),
        DisplayAttributeDataCapable::addAdditionalDisplayAttributeData,
        valueOutput);
    this.handleEasyNPCData(
        "faction",
        this.getEasyNPCFactionData(),
        FactionDataCapable::addAdditionalFactionData,
        valueOutput);
    this.handleEasyNPCData(
        "inventory",
        this.getEasyNPCInventoryData(),
        InventoryDataCapable::addAdditionalInventoryData,
        valueOutput);
    this.handleEasyNPCData(
        "model", this.getEasyNPCModelData(), ModelDataCapable::addAdditionalModelData, valueOutput);
    this.handleEasyNPCData(
        "navigation",
        this.getEasyNPCNavigationData(),
        NavigationDataCapable::addAdditionalNavigationData,
        valueOutput);
    this.handleEasyNPCData(
        "objective",
        this.getEasyNPCObjectiveData(),
        ObjectiveDataCapable::addAdditionalObjectiveData,
        valueOutput);
    this.handleEasyNPCData(
        "owner", this.getEasyNPCOwnerData(), OwnerDataCapable::addAdditionalOwnerData, valueOutput);
    this.handleEasyNPCData(
        "preset",
        this.getEasyNPCPresetData(),
        PresetDataCapable::addAdditionalPresetData,
        valueOutput);
    this.handleEasyNPCData(
        "profession",
        this.getEasyNPCProfessionData(),
        ProfessionDataCapable::addAdditionalProfessionData,
        valueOutput);
    this.handleEasyNPCData(
        "progression",
        this.getEasyNPCProgressionData(),
        ProgressionDataCapable::addAdditionalProgressionData,
        valueOutput);
    this.handleEasyNPCData(
        "render",
        this.getEasyNPCRenderData(),
        RenderDataCapable::addAdditionalRenderData,
        valueOutput);
    this.handleEasyNPCData(
        "skin", this.getEasyNPCSkinData(), SkinDataCapable::addAdditionalSkinData, valueOutput);
    this.handleEasyNPCData(
        "sound", this.getEasyNPCSoundData(), SoundDataCapable::addAdditionalSoundData, valueOutput);
    this.handleEasyNPCData(
        "state", this.getEasyNPCStateData(), StateDataCapable::addAdditionalStateData, valueOutput);
    this.handleEasyNPCData(
        "status",
        this.getEasyNPCStatusData(),
        StatusDataCapable::addAdditionalStatusData,
        valueOutput);
    this.handleEasyNPCData(
        "trading",
        this.getEasyNPCTradingData(),
        TradingDataCapable::addAdditionalTradingData,
        valueOutput);
    this.handleEasyNPCData(
        "variant",
        this.getEasyNPCVariantData(),
        VariantDataCapable::addAdditionalVariantData,
        valueOutput);
  }

  default void readEasyNPCBaseAdditionalSaveData(
      ValueInput valueInput, HolderLookup.Provider provider) {
    // First read important data to ensure that all other data can be linked to the variant.
    this.handleEasyNPCData(
        "config",
        this.getEasyNPCConfigData(),
        ConfigDataCapable::readAdditionalConfigData,
        valueInput);
    this.handleEasyNPCData(
        "variant",
        this.getEasyNPCVariantData(),
        VariantDataCapable::readAdditionalVariantData,
        valueInput);

    ActionEventDataCapable<E> actionEventData = this.getEasyNPCActionEventData();
    this.handleEasyNPCData(
        "action event",
        actionEventData,
        ActionEventDataCapable::readAdditionalActionData,
        valueInput);
    this.handleEasyNPCData(
        "attack",
        this.getEasyNPCAttackData(),
        AttackDataCapable::readAdditionalAttackData,
        valueInput);
    this.handleEasyNPCData(
        "attribute",
        this.getEasyNPCAttributeData(),
        AttributeDataCapable::readAdditionalAttributeData,
        valueInput);
    DialogDataCapable<E> dialogData = this.getEasyNPCDialogData();
    this.handleEasyNPCData(
        "dialog", dialogData, DialogDataCapable::readAdditionalDialogData, valueInput);
    this.handleEasyNPCData(
        "display attribute",
        this.getEasyNPCDisplayAttributeData(),
        DisplayAttributeDataCapable::readAdditionalDisplayAttributeData,
        valueInput);
    this.handleEasyNPCData(
        "faction",
        this.getEasyNPCFactionData(),
        FactionDataCapable::readAdditionalFactionData,
        valueInput);
    this.handleEasyNPCData(
        "inventory",
        this.getEasyNPCInventoryData(),
        InventoryDataCapable::readAdditionalInventoryData,
        valueInput);
    this.handleEasyNPCData(
        "model", this.getEasyNPCModelData(), ModelDataCapable::readAdditionalModelData, valueInput);
    NavigationDataCapable<E> navigationData = this.getEasyNPCNavigationData();
    this.handleEasyNPCData(
        "navigation",
        navigationData,
        NavigationDataCapable::readAdditionalNavigationData,
        valueInput);
    this.handleEasyNPCData(
        "owner", this.getEasyNPCOwnerData(), OwnerDataCapable::readAdditionalOwnerData, valueInput);
    this.handleEasyNPCData(
        "preset",
        this.getEasyNPCPresetData(),
        PresetDataCapable::readAdditionalPresetData,
        valueInput);
    this.handleEasyNPCData(
        "profession",
        this.getEasyNPCProfessionData(),
        ProfessionDataCapable::readAdditionalProfessionData,
        valueInput);
    this.handleEasyNPCData(
        "progression",
        this.getEasyNPCProgressionData(),
        ProgressionDataCapable::readAdditionalProgressionData,
        valueInput);
    this.handleEasyNPCData(
        "render",
        this.getEasyNPCRenderData(),
        RenderDataCapable::readAdditionalRenderData,
        valueInput);
    this.handleEasyNPCData(
        "skin", this.getEasyNPCSkinData(), SkinDataCapable::readAdditionalSkinData, valueInput);
    this.handleEasyNPCData(
        "sound", this.getEasyNPCSoundData(), SoundDataCapable::readAdditionalSoundData, valueInput);
    this.handleEasyNPCData(
        "state", this.getEasyNPCStateData(), StateDataCapable::readAdditionalStateData, valueInput);
    StatusDataCapable<E> statusData = this.getEasyNPCStatusData();
    this.handleEasyNPCData(
        "status", statusData, StatusDataCapable::readAdditionalStatusData, valueInput);
    TradingDataCapable<E> tradingData = this.getEasyNPCTradingData();
    this.handleEasyNPCData(
        "trading", tradingData, TradingDataCapable::readAdditionalTradingData, valueInput);
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
        valueInput);

    this.handleEasyNPCData(
        "navigation refresh", navigationData, NavigationDataCapable::refreshNavigation);

    if (statusData != null) {
      boolean replacesRunningNPCData = this.isAddedToServerLevel();
      this.handleEasyNPCData(
          "status tracking",
          () -> statusData.trackLoadedNPCData(valueInput, replacesRunningNPCData));
    }

    this.getMob().setPersistenceRequired();
  }

  private <D> void ifPresent(D data, Consumer<D> dataOperation) {
    if (data != null) {
      dataOperation.accept(data);
    }
  }

  private <D, A> void ifPresent(D data, BiConsumer<D, A> dataOperation, A argument) {
    if (data != null) {
      dataOperation.accept(data, argument);
    }
  }

  private <D> void handleEasyNPCData(String dataName, D data, Consumer<D> dataOperation) {
    if (data != null) {
      this.handleEasyNPCData(dataName, () -> dataOperation.accept(data));
    }
  }

  private <D, A> void handleEasyNPCData(
      String dataName, D data, BiConsumer<D, A> dataOperation, A argument) {
    if (data != null) {
      this.handleEasyNPCData(dataName, () -> dataOperation.accept(data, argument));
    }
  }

  private boolean isAddedToServerLevel() {
    Entity entity = this.getEntity();
    return entity.level() instanceof ServerLevel serverLevel
        && serverLevel.getEntity(entity.getId()) == entity;
  }
}
